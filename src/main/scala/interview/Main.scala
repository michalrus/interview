package interview

import akka.actor.ActorSystem
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{
  DJKeyGenParameterSpec,
  ScDamgardJurikEnc
}
import java.security.SecureRandom

object Main extends App {

  val rng       = new SecureRandom()
  val encryptor = new ScDamgardJurikEnc(rng)
  val aliceKeys, bobKeys, carrollKeys =
    encryptor.generateKey(new DJKeyGenParameterSpec(128, 40))

  val system = ActorSystem()

  val alice   = system.actorOf(User.props(aliceKeys), "alice")
  val bob     = system.actorOf(User.props(bobKeys), "bob")
  val carroll = system.actorOf(Broker.props(carrollKeys), "carroll")

  val environment =
    system.actorOf(Environment.props, "environment")

  // now, somehow the parties are chosen; we have them artificially
  // hardcoded for the sake of this example
  environment ! Environment.Init(carroll, Broker.Parties(alice, bob))

}
