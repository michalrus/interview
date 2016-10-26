package interview

import akka.actor.{Actor, ActorRef, Props}
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.damgardJurikProduct.{
  SigmaDJProductCommonInput,
  SigmaDJProductVerifierComputation
}
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.utility.SigmaProtocolMsg
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.DamgardJurikPublicKey
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.ScDamgardJurikEnc
import java.security.{PublicKey, SecureRandom}
import java.util.UUID

import Broker.EncryptedMsg

object User {
  def props(verificationPublicKey: PublicKey) =
    Props(new User(verificationPublicKey))

  final case class Session(
      secret: BigInt,
      cA: Option[EncryptedMsg],
      cB: Option[EncryptedMsg],
      cC: Option[EncryptedMsg],
      verifierComputation: Option[SigmaDJProductVerifierComputation])

  final case class Invite(session: UUID, broker: ActorRef)

  final case class ProtocolAbort(session: UUID)

  final case class BothCipherTexts(session: UUID,
                                   cA: EncryptedMsg,
                                   cB: EncryptedMsg,
                                   broker: ActorRef,
                                   brokerKey: PublicKey)

  final case class EncryptedProduct(session: UUID,
                                    cC: EncryptedMsg,
                                    broker: ActorRef)

  final case class Proof(session: UUID,
                         a: SigmaProtocolMsg,
                         z: SigmaProtocolMsg)
}

import User._
import Broker.{calculateSoundness, encrypt}

final class User(verificationPublicKey: PublicKey)
    extends Actor
    with WithSession {

  val rng       = new SecureRandom
  val encryptor = new ScDamgardJurikEnc(rng)
  encryptor.setLengthParameter(MagicNumbers.LengthParameter)

  def receive = state(Map.empty)

  def state(sessions: Map[UUID, Session]): Receive = {

    case Invite(sessionId, broker) ⇒
      /* spec-1° */
      val secret = BigInt(rng.nextInt).abs // has to be ≥ 0
      val msg    = encrypt(encryptor, verificationPublicKey, secret) // encrypting for later verification
      /* spec-2° */
      sender() ! Environment.Tell(broker,
                                  Broker.EncryptedSecret(sessionId, self, msg))
      context.become(
        state(
          sessions + (sessionId → Session(secret, None, None, None, None))))

    case ProtocolAbort(sessionId) ⇒
    /* handle timeout somehow */

    case BothCipherTexts(sessionId, cA, cB, broker, brokerKey) ⇒
      /* spec-3° */
      withSession(sessions, sessionId, state _) { session ⇒
        if (session.cA.isDefined || session.cB.isDefined) {
          /* handle the wicked situation somehow */
        }

        /* spec-4° */
        val msg = encrypt(encryptor, brokerKey, session.secret)
        sender() ! Environment
          .Tell(broker, Broker.DecryptableSecret(sessionId, self, msg))

        Some(session.copy(cA = Some(cA), cB = Some(cB)))
      }

    case EncryptedProduct(sessionId, product, broker) ⇒
      /* spec-5° */
      withSession(sessions, sessionId, state _) { session ⇒
        val newSession = session.copy(cC = Some(product))
        Some(requestProof(newSession, sessionId, broker))
      }

    case Proof(sessionId, a, z) ⇒
      withSession(sessions, sessionId, state _) { session ⇒
        (session.cA zip session.cB zip session.cC zip session.verifierComputation).headOption foreach {
          case (((cA, cB), cC), computation) ⇒
            @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
            val input = new SigmaDJProductCommonInput(
              verificationPublicKey
                .asInstanceOf[DamgardJurikPublicKey], // :crying-cat:
              cA.cipherText,
              cB.cipherText,
              cC.cipherText)
            val result = computation.verify(input, a, z)
            sender() ! Environment.VerificationResult(sessionId, result)
        }
        Some(session)
      }

  }

  def requestProof(session: Session,
                   sessionId: UUID,
                   broker: ActorRef): Session = {
    val newSession = (session.cA zip session.cB zip session.cC).headOption map {
      case ((cA, cB), cC) ⇒
        val t = calculateSoundness(verificationPublicKey)

        val computation =
          new SigmaDJProductVerifierComputation(t,
                                                MagicNumbers.LengthParameter,
                                                rng)

        computation.sampleChallenge()
        val challenge = computation.getChallenge
        sender() ! Environment.Tell(
          broker,
          Broker.ProofRequest(sessionId, self, cA, cB, cC, challenge))

        session.copy(verifierComputation = Some(computation))
    }
    newSession getOrElse session
  }

}
