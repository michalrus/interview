package interview

import akka.actor.{Actor, ActorRef, Props}
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.damgardJurikProduct.{
  SigmaDJProductProverComputation,
  SigmaDJProductProverInput
}
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.ScDamgardJurikEnc
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.DamgardJurikPublicKey
import edu.biu.scapi.midLayer.ciphertext.BigIntegerCiphertext
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText
import java.security.{KeyPair, PublicKey, SecureRandom}
import java.util.UUID

object Broker {
  def props(keys: KeyPair, verificationPublicKey: PublicKey) = Props(new Broker(keys, verificationPublicKey))

  /** `r` is the random integer chosen for encryption of `cipherText` */
  final case class EncryptedMsg(cipherText: BigIntegerCiphertext, r: BigInt)

  final case class Session(parties: Parties,
                           cipherA: Option[EncryptedMsg],
                           cipherB: Option[EncryptedMsg],
                           plainA: Option[BigInt],
                           plainB: Option[BigInt])

  final case class Parties(userA: ActorRef, userB: ActorRef)
  final case class EncryptedSecret(session: UUID,
                                   party: ActorRef,
                                   cipherText: EncryptedMsg)
  final case class DecryptableSecret(session: UUID,
                                     party: ActorRef,
                                     cipherText: EncryptedMsg)
  final case class ProofRequest(session: UUID,
                                party: ActorRef,
                                cipherA: EncryptedMsg,
                                cipherB: EncryptedMsg,
                                cipherC: EncryptedMsg,
                                challenge: Array[Byte])

  // According to the Cookbook, soundness has to be < |n|/3:
  def calculateSoundness(k: PublicKey): Int =
    k match {
      case djpk: DamgardJurikPublicKey ⇒ djpk.getModulus.bitLength / 3 - 1
      case _                           ⇒ -1
    }

  // Encrypting with less ceremony.
  def encrypt(encryptor: ScDamgardJurikEnc,
              key: PublicKey,
              secret: BigInt): EncryptedMsg = {
    encryptor.setKey(key)
    val plainText = new BigIntegerPlainText(secret.underlying)
    val r         = encryptor.generateEncryptionRandomness
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    val cipherText =
      encryptor
        .encrypt(plainText, r)
        .asInstanceOf[BigIntegerCiphertext] // :crying-cat:
    EncryptedMsg(cipherText, r)
  }

  def decrypt(encryptor: ScDamgardJurikEnc,
    keys: KeyPair,
    cipherText: EncryptedMsg): BigInt = {
    encryptor.setKey(keys.getPublic, keys.getPrivate)
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    val plainText = encryptor.decrypt(cipherText.cipherText)
      .asInstanceOf[BigIntegerPlainText] // :crying-cat:
    plainText.getX
  }

}

trait WithSession { self: Actor ⇒ // DRY hard with Actors?
  /** Runs an `action` in the context of current communication
    * session.
    */
  def withSession[S](
      sessions: Map[UUID, S],
      id: UUID,
      nextBehavior: Map[UUID, S] ⇒ Actor.Receive)(action: S ⇒ S): Unit =
    sessions.get(id).foreach { session ⇒
      val ns = action(session)
      context.become(nextBehavior(sessions + (id → ns)))
    }
}

import Broker._

final class Broker(myKeys: KeyPair, verificationPublicKey: PublicKey) extends Actor with WithSession {

  val rng       = new SecureRandom
  val encryptor = new ScDamgardJurikEnc(rng)
  encryptor.setLengthParameter(MagicNumbers.LengthParameter)

  def receive = state(Map.empty)

  def state(sessions: Map[UUID, Session]): Receive = {

    case ps @ Parties(a, b) ⇒
      val id = UUID.randomUUID
      /* spec-1° */
      List(a, b).foreach(p ⇒
        sender() ! Environment.Tell(p, User.Invite(id, self)))
      context.become(
        state(sessions + (id → Session(ps, None, None, None, None))))

    case EncryptedSecret(sessionId, party, cipher) ⇒
      /* spec-2° */
      withSession(sessions, sessionId, state _) { session ⇒
        collectFromBoth(session, party, cipher)(
          _.cipherA,
          _.cipherB,
          (s, v) ⇒ s.copy(cipherA = v),
          (s, v) ⇒ s.copy(cipherB = v),
          /* spec-3° */
          (cA,
           cB) ⇒ User.BothCipherTexts(sessionId, cA, cB, self, myKeys.getPublic))
      }

    case DecryptableSecret(sessionId, party, cipher) ⇒
      /* spec-4° */
      withSession(sessions, sessionId, state _) { session ⇒
        /* spec-5° */
        val plainText = decrypt(encryptor, myKeys, cipher)
        collectFromBoth(session, party, plainText)(
          _.plainA,
          _.plainB,
          (s, v) ⇒ s.copy(plainA = v),
          (s, v) ⇒ s.copy(plainB = v), { (a, b) ⇒
            val product = a * b
            val msg     = encrypt(encryptor, verificationPublicKey, product) // encrypting for verification
            User.EncryptedProduct(sessionId, msg, self)
          })
      }

    case ProofRequest(sessionId, party, cA, cB, cC, challenge) ⇒
      withSession(sessions, sessionId, state _) { session ⇒
        session.plainA zip session.plainB foreach {
          case (xA, xB) ⇒
            val computation = new SigmaDJProductProverComputation(
              calculateSoundness(verificationPublicKey),
              MagicNumbers.LengthParameter,
              rng)

            val input = new SigmaDJProductProverInput(
              verificationPublicKey
                .asInstanceOf[DamgardJurikPublicKey], // :crying-cat:
              cA.cipherText,
              cB.cipherText,
              cC.cipherText,
              cA.r.underlying,
              cB.r.underlying,
              cC.r.underlying,
              new BigIntegerPlainText(xA.underlying),
              new BigIntegerPlainText(xB.underlying))

            val a = computation.computeFirstMsg(input)
            val z = computation.computeSecondMsg(challenge)
            sender() ! Environment.Tell(party, User.Proof(sessionId, a, z))
        }
        session
      }

  }

  /** Collects value of type `V` from both `Parties`, saves it to
    * current `Session`, and when both are present, sends a message
    * `M` to both parties.
    */
  def collectFromBoth[V, M](session: Session,
                            senderParty: ActorRef,
                            newValue: V)(
      readA: Session ⇒ Option[V],
      readB: Session ⇒ Option[V],
      writeA: (Session, Option[V]) ⇒ Session,
      writeB: (Session, Option[V]) ⇒ Session,
      tellBoth: (V, V) ⇒ M): Session = {
    val newSession =
      if (senderParty == session.parties.userA && readA(session).isEmpty) {
        writeA(session, Some(newValue))
      } else if (senderParty == session.parties.userB && readB(session).isEmpty) {
        writeB(session, Some(newValue))
      } else {
        /* handle the wicked situation somehow */
        session
      }

    readA(newSession) zip readB(newSession) foreach {
      case (va, vb) ⇒
        List(newSession.parties.userA, newSession.parties.userB).foreach(p ⇒
          sender() ! Environment.Tell(p, tellBoth(va, vb)))
    }

    newSession
  }

}
