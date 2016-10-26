package interview

import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{
  DJKeyGenParameterSpec,
  ScDamgardJurikEnc
}
import java.security.{KeyPair, SecureRandom}
import org.scalacheck.{Arbitrary, Gen}

object BrokerSpec {

  import Gen.const

  val rng = new SecureRandom

  val genKeys: Gen[KeyPair] = for {
    encryptor ← Gen.const(new ScDamgardJurikEnc(rng))
    _         ← encryptor.setLengthParameter(MagicNumbers.LengthParameter)
    keys ← encryptor.generateKey(
      new DJKeyGenParameterSpec(MagicNumbers.KeyModulusLength,
                                MagicNumbers.KeyPrimenessCertainty))
  } yield keys

  implicit val arbKeys: Arbitrary[KeyPair] = Arbitrary(genKeys)

}

import BrokerSpec.{genKeys, rng}

class BrokerSpec extends UnitSpec {

  "Encryption" should {
    "allow decryption" in forAll(Gen.posNum[Int], genKeys) { (secret, keys) ⇒
      val encryptor = new ScDamgardJurikEnc(rng)
      encryptor.setLengthParameter(MagicNumbers.LengthParameter)
      val plainText = BigInt(secret.abs)
      val cipherText =
        Broker.encrypt(encryptor, keys.getPublic, plainText)
      plainText shouldEqual Broker.decrypt(encryptor, keys, cipherText)
    }
  }
}
