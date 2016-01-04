package sBitTorrent

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.io.{UdpConnected, IO, Udp}
import akka.util.ByteString

class DHTNode(val remoteIP: String, val remotePort: Int) extends Actor with ActorLogging {
    import context.system

    import DHTNode.port
    val local = new InetSocketAddress("127.0.0.1", port)
    val remote = new InetSocketAddress(remoteIP, remotePort)

    IO(UdpConnected) ! UdpConnected.Connect(self, remote)

    override def receive = {
        case UdpConnected.Connected =>
            context.become(ready(sender()))
        case others =>
            log.error(s"early message ${others}")
    }

    def ready(socket: ActorRef): Receive = {
        case msg: String =>
            println(s"client send ${msg}")
            socket ! UdpConnected.Send(ByteString(msg))
        case UdpConnected.Received(data) =>
            println(s"client receive ${data.utf8String}")
        case UdpConnected.Disconnect => socket ! UdpConnected.Disconnect
        case UdpConnected.Disconnected => context.stop(self)
        case others => println(others)
    }
}

object DHTNode {
    val port = 6969
    val digest = java.security.MessageDigest.getInstance("SHA-1")
    digest.update("ganwenchuang".getBytes())
    val nodeID = new String(digest.digest())
//    val nodeID = "abcdefghij0123456789"

    def main(args: Array[String]) = {
        val bencode = Bencode.Bencode()
        val remoteIP = "212.129.33.50"
//        val remoteIP = "127.0.0.1"
        val remotePort = 6881

        val system = ActorSystem("DHT")
        val node = system.actorOf(Props(classOf[DHTNode], remoteIP, remotePort), "node")
        println("nodeID: " + nodeID)
        val ping = Map("t" -> "aa", "y" -> "q", "q" -> "ping", "a" -> Map("id" -> nodeID))
        val msg = bencode.encode(ping)

        TimeUnit.SECONDS.sleep(1)
        node ! msg
    }
}
