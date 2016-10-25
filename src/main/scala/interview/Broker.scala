package interview

import akka.actor.{Actor, ActorRef, Props}
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.ScDamgardJurikEnc
import edu.biu.scapi.midLayer.ciphertext.AsymmetricCiphertext
import edu.biu.scapi.midLayer.plaintext.{BigIntegerPlainText, Plaintext}
import java.security.{KeyPair, SecureRandom}
import java.util.UUID

object Broker {
  def props(keys: KeyPair) = Props(new Broker(keys))

  final case class Session(parties: Parties,
                           cipherA: Option[AsymmetricCiphertext],
                           cipherB: Option[AsymmetricCiphertext],
                           plainA: Option[BigInt],
                           plainB: Option[BigInt])

  final case class Parties(userA: ActorRef, userB: ActorRef)
  final case class EncryptedSecret(session: UUID,
                                   party: ActorRef,
                                   cipherText: AsymmetricCiphertext)
  final case class DecryptableSecret(session: UUID,
                                     party: ActorRef,
                                     cipherText: AsymmetricCiphertext)
}

import Broker._

final class Broker(keys: KeyPair) extends Actor {

  val rng       = new SecureRandom
  val encryptor = new ScDamgardJurikEnc(rng)

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
      withSession(sessions, sessionId) { session ⇒
        collectFromBoth(session, party, Some(cipher))(
          _.cipherA,
          _.cipherB,
          (s, v) ⇒ s.copy(cipherA = v),
          (s, v) ⇒ s.copy(cipherB = v),
          /* spec-3° */
          (cA,
           cB) ⇒ User.BothCipherTexts(sessionId, cA, cB, self, keys.getPublic))
      }

    case DecryptableSecret(sessionId, party, cipher) ⇒
      /* spec-4° */
      withSession(sessions, sessionId) { session ⇒
        /* spec-5° */
        encryptor.setKey(keys.getPublic, keys.getPrivate)
        val plainText = toBigInt(encryptor.decrypt(cipher))
        collectFromBoth(session, party, plainText)(
          _.plainA,
          _.plainB,
          (s, v) ⇒ s.copy(plainA = v),
          (s, v) ⇒ s.copy(plainB = v), { (a, b) ⇒
            encryptor.setKey(keys.getPublic) // encrypting for «me»
            val product    = a * b
            val plainText  = new BigIntegerPlainText(product.underlying)
            val cipherText = encryptor.encrypt(plainText)
            User.EncryptedProduct(sessionId, cipherText, self)
          })
      }

  }

  /** Runs an `action` in the context of current communication
    * session.
    */
  def withSession(sessions: Map[UUID, Session], id: UUID)(
      action: Session ⇒ Session): Unit =
    sessions.get(id).foreach { session ⇒
      val ns = action(session)
      context.become(state(sessions + (id → ns)))
    }

  /** Collects value of type `V` from both `Parties`, saves it to
    * current `Session`, and when both are present, sends a message
    * `M` to both parties.
    */
  def collectFromBoth[V, M](session: Session,
                            senderParty: ActorRef,
                            newValue: Option[V])(
      readA: Session ⇒ Option[V],
      readB: Session ⇒ Option[V],
      writeA: (Session, Option[V]) ⇒ Session,
      writeB: (Session, Option[V]) ⇒ Session,
      tellBoth: (V, V) ⇒ M): Session = {
    val newSession =
      if (senderParty == session.parties.userA && readA(session).isEmpty) {
        writeA(session, newValue)
      } else if (senderParty == session.parties.userB && readB(session).isEmpty) {
        writeB(session, newValue)
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

  def toBigInt(pt: Plaintext): Option[BigInt] =
    pt match {
      case bipt: BigIntegerPlainText ⇒ Some(bipt.getX)
      case _                         ⇒ None
    }

}
