package interview

import akka.actor.ActorSystem
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{
  DJKeyGenParameterSpec,
  ScDamgardJurikEnc
}
import java.security.SecureRandom
import scala.concurrent.duration._

object MagicNumbers {

  // sample values, taken from scapi docs
  val KeyModulusLength      = 128
  val KeyPrimenessCertainty = 40

  // If not given explicitly, the length param is calculated according
  // to this formula:
  //
  //   |plain| / (|publicKey| - 1) + 1
  //
  // Since we’re encrypting 31-bit integers, this would always end up
  // at just 1 (Paillier’s scheme). Let’s force it to something more
  // exciting.
  val LengthParameter = 11

}

object Main extends App {

  val rng       = new SecureRandom()
  val encryptor = new ScDamgardJurikEnc(rng)

  encryptor.setLengthParameter(MagicNumbers.LengthParameter)
  val carrollKeys, verificationKeys =
    encryptor.generateKey(
      new DJKeyGenParameterSpec(MagicNumbers.KeyModulusLength,
                                MagicNumbers.KeyPrimenessCertainty))

  /** This public key (and *only* public, private can be dumped!) is
    * used by all parties for public announcements and verifications
    * (points 2, 3, 5 of the spec). Its cipher texts are never
    * directly decrypted. See README.md.
    */
  val verificationPublicKey = verificationKeys.getPublic

  val system = ActorSystem()

  val alice =
    system.actorOf(User.props(verificationPublicKey), "alice")
  val bob = system.actorOf(User.props(verificationPublicKey), "bob")
  val carroll =
    system.actorOf(Broker.props(carrollKeys, verificationPublicKey, 1.second),
                   "carroll")

  val environment =
    system.actorOf(Environment.props, "environment")

  // now, somehow the parties are chosen; we have them artificially
  // hardcoded for the sake of this example
  environment ! Environment.Init(carroll, Broker.Parties(alice, bob))

}
