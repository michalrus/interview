package interview

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.damgardJurikProduct.{
  SigmaDJProductProverComputation,
  SigmaDJProductProverInput
}
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{
  DJKeyGenParameterSpec,
  ScDamgardJurikEnc
}
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.DamgardJurikPublicKey
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText
import java.security.SecureRandom
import java.util.UUID
import scala.concurrent.duration._

import Broker.{encrypt, decrypt, calculateSoundness}

class UserSpec
    extends TestKit(ActorSystem())
    with ImplicitSender
    with UnitSpec {

  "User" should {
    "follow the protocol" in {
      val rng       = new SecureRandom()
      val encryptor = new ScDamgardJurikEnc(rng)

      encryptor.setLengthParameter(MagicNumbers.LengthParameter)
      val brokerKeys, verificationKeys =
        encryptor.generateKey(
          new DJKeyGenParameterSpec(MagicNumbers.KeyModulusLength,
                                    MagicNumbers.KeyPrimenessCertainty))

      val verificationPublicKey = verificationKeys.getPublic

      val user        = system.actorOf(User.props(verificationPublicKey))
      val brokerProbe = TestProbe() // we’re pretending to be Environment, a fake broker is needed
      val broker      = brokerProbe.ref
      val sessionId   = UUID.randomUUID

      // spec-1°
      user ! User.Invite(sessionId, broker)

      // spec-2°
      val cA = expectMsgPF(100.millis) {
        case Environment.Tell(
            `broker`,
            Broker.EncryptedSecret(`sessionId`, `user`, cipherText)) ⇒
          cipherText
      }
      val xB = BigInt(rng.nextInt).abs
      val cB = encrypt(encryptor, verificationPublicKey, xB)

      // spec-3°
      user ! User
        .BothCipherTexts(sessionId, cA, cB, broker, brokerKeys.getPublic)

      // spec-4°
      val xA = expectMsgPF(100.millis) {
        case Environment.Tell(
            `broker`,
            Broker.DecryptableSecret(`sessionId`, `user`, msg)) ⇒
          decrypt(encryptor, brokerKeys, msg)
      }

      // spec-5°
      val xC = xA * xB
      val cC = encrypt(encryptor, verificationPublicKey, xC)
      user ! User.EncryptedProduct(sessionId, cC, broker)

      // verification
      val challenge = expectMsgPF(100.millis) {
        case Environment.Tell(`broker`,
                              Broker.ProofRequest(`sessionId`,
                                                  `user`,
                                                  `cA`,
                                                  `cB`,
                                                  `cC`,
                                                  challenge)) ⇒
          challenge
      }

      val computation = new SigmaDJProductProverComputation(
        calculateSoundness(verificationPublicKey),
        MagicNumbers.LengthParameter,
        rng)

      val input = new SigmaDJProductProverInput(
        verificationPublicKey.asInstanceOf[DamgardJurikPublicKey],
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
      user ! User.Proof(sessionId, a, z)

      val result = expectMsgPF(100.millis) {
        case Environment.VerificationResult(`sessionId`, r) ⇒ r
      }

      system.stop(user)

      result shouldEqual true
    }
  }

}
