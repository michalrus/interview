package interview

import akka.actor.{Actor, ActorRef, Props}
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.ScDamgardJurikEnc
import edu.biu.scapi.midLayer.ciphertext.AsymmetricCiphertext
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText
import java.security.{KeyPair, PublicKey, SecureRandom}
import java.util.UUID

object User {
  def props(keys: KeyPair) = Props(new User(keys))

  final case class Session(secret: BigInt,
                           cA: Option[AsymmetricCiphertext],
                           cB: Option[AsymmetricCiphertext])

  final case class Invite(session: UUID, broker: ActorRef)

  final case class BothCipherTexts(session: UUID,
                                   cA: AsymmetricCiphertext,
                                   cB: AsymmetricCiphertext,
                                   broker: ActorRef,
                                   brokerKey: PublicKey)

  final case class EncryptedProduct(session: UUID,
                                    cC: AsymmetricCiphertext,
                                    broker: ActorRef)
}

import User._

final class User(keys: KeyPair) extends Actor {

  val rng       = new SecureRandom
  val encryptor = new ScDamgardJurikEnc(rng)

  def receive = state(Map.empty)

  def state(sessions: Map[UUID, Session]): Receive = {

    case Invite(sessionId, broker) ⇒
      /* spec-1° */
      val secret = BigInt(rng.nextInt).abs // has to be ≥ 0
      encryptor.setKey(keys.getPublic) // encrypting «for me»
      val plainText  = new BigIntegerPlainText(secret.underlying)
      val cipherText = encryptor.encrypt(plainText)
      /* spec-2° */
      sender() ! Environment
        .Tell(broker, Broker.EncryptedSecret(sessionId, self, cipherText))
      context.become(
        state(sessions + (sessionId → Session(secret, None, None))))

    case BothCipherTexts(sessionId, cA, cB, broker, brokerKey) ⇒
      /* spec-3° */
      sessions.get(sessionId).foreach { session ⇒
        if (session.cA.isDefined || session.cB.isDefined) {
          /* handle the wicked situation somehow */
        }

        /* spec-4° */
        encryptor.setKey(brokerKey)
        val plainText  = new BigIntegerPlainText(session.secret.underlying)
        val cipherText = encryptor.encrypt(plainText)
        sender() ! Environment
          .Tell(broker, Broker.DecryptableSecret(sessionId, self, cipherText))

        val newSession = session.copy(cA = Some(cA), cB = Some(cB))
        context.become(state(sessions + (sessionId → newSession)))
      }

    case EncryptedProduct(sessionId, product, broker) ⇒
      /* spec-5° */
      // TODO: Verify!
      val _ = context.system.terminate()

  }

}
