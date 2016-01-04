package sBitTorrent

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.io.IO
import akka.io.Udp
import java.net.InetSocketAddress
import akka.util.ByteString
import akka.io.UdpConnected

//#sender
class SimpleSender(remote: InetSocketAddress) extends Actor with ActorLogging {
    import context.system
    IO(Udp) ! Udp.SimpleSender

    def receive = {
        case Udp.SimpleSenderReady =>
            context.become(ready(sender()))
        case others => log.error(s"early message ${others}")
    }

    def ready(send: ActorRef): Receive = {
        case msg: String =>
            log.info(s"SimpleSender receive ${msg}")
            send ! Udp.Send(ByteString(msg), remote)
            if (msg == "world") send ! PoisonPill
        case Udp.Received(data, peer) =>
            log.info(s"sender receive ${data.utf8String} from ${peer.getAddress}:${peer.getPort}")
        case others => log.error(s"unexpected message ${others}")
    }
}

//#listener
class Listener(nextActor: ActorRef) extends Actor {
    import context.system
    IO(Udp) ! Udp.Bind(self, new InetSocketAddress("localhost", 6969))

    def receive = {
        case Udp.Bound(local) =>
            context.become(ready(sender()))
    }

    def ready(socket: ActorRef): Receive = {
        case Udp.Received(data, remote) =>
            println(s"listener receive ${data.utf8String} from ${remote.getAddress}:${remote.getPort}")
            val processed = data.utf8String
            socket ! Udp.Send(data, remote)
            nextActor ! processed
        case Udp.Unbind  =>
            socket ! Udp.Unbind
        case Udp.Unbound => context.stop(self)
        case msg: String =>
            println(s"listener receive ${msg} from outside")
    }
}
//#listener

//#connected
class Connected(remote: InetSocketAddress) extends Actor {
    import context.system
    IO(UdpConnected) ! UdpConnected.Connect(self, remote)

    def receive = {
        case UdpConnected.Connected =>
            context.become(ready(sender()))
            sender() ! UdpConnected.Send(ByteString("hello"))
    }

    def ready(connection: ActorRef): Receive = {
        case UdpConnected.Received(data) =>
            if (data.utf8String == "hello")
                connection ! UdpConnected.Send(ByteString("world"))
        case msg: String =>
            connection ! UdpConnected.Send(ByteString(msg))
        case d @ UdpConnected.Disconnect => connection ! d
        case UdpConnected.Disconnected   => context.stop(self)
    }
}

class Print extends Actor {
    override def receive = {
        case msg: String =>
            println(s"printer receive $msg from ${sender()}")
    }
}

object ScalaUdpDocSpec {
    def main(args: Array[String]) = {
        val system = ActorSystem("udpTest")
        val remote = new InetSocketAddress("127.0.0.1", 6969)
//        val sender = system.actorOf(Props(classOf[SimpleSender], remote), "sender")
        val printer = system.actorOf(Props[Print], "printer")
        val listener = system.actorOf(Props(classOf[Listener], printer), "listener")

//        TimeUnit.SECONDS.sleep(1)
//        sender ! "haha"
//        sender ! "world"
//        listener ! Udp.Unbind
    }
}