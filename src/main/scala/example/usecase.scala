package example

import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.util.chaining._

object usecase {
  import model._

  abstract class ConsoleChat {
    def run(): Unit
  }

  final class ConsoleChatImpl(
      postMessage: PostMessage,
      fetchMessages: FetchMessages
  ) extends ConsoleChat {
    import ConsoleChatImpl._
    override def run(): Unit =
      Iterator
        .continually(StdIn.readLine())
        .takeWhile(!quit.matches(_))
        .map {
          case post(name, msg) =>
            postMessage.run(name, msg).map {
              case PostMessage.Result.Success        => "[送信に成功しました]"
              case PostMessage.Result.MemberNotFound => "[メンバー登録してください]"
            }
          case fetch(name) =>
            fetchMessages.run(name).map {
              case FetchMessages.Result.Success(m) =>
                "=====\n" + m
                  .map { n =>
                    s"${n.name}「${n.message}」(既読 ${n.readCount})"
                  }
                  .mkString("\n")
              case FetchMessages.Result.MemberNotFound => "[メンバー登録してください]"
            }
          case input =>
            s"[不正な入力です: $input]"
        }
        .foreach(println)
//        .foreach {
//          case Success(v) => println(v)
//          case Failure(_) => println("[処理に失敗しました]")
//        }
  }
  object ConsoleChatImpl {
    val quit = """:quit|:q""".r
    val post = """\s*(\S+)\s+post\s+([\S|\s]+)""".r
    val fetch = """\s*(\S+)\s+fetch\s*""".r
  }

  // ===

  abstract class GetCurrentTimeMillis {
    def run(): EpochMillis
  }
  final class GetCurrentTimeMillisImpl extends GetCurrentTimeMillis {
    override def run(): EpochMillis = System.currentTimeMillis()
  }

  abstract class GetMemberName {
    def run(memberId: MemberId): Try[String]
  }
  final class GetMemberNameImpl(memberRepository: MemberRepository) extends GetMemberName {
    override def run(memberId: MemberId): Try[String] =
      memberRepository.resolveById(memberId).map {
        case Some(n) => n.name
        case None    => "<削除済みメンバー>"
      }

  }

  // ===

  abstract class PostMessage {
    def run(name: String, message: String): Try[PostMessage.Result]
  }
  object PostMessage {
    abstract class Result extends Product with Serializable
    object Result {
      case object Success extends Result
      case object MemberNotFound extends Result
    }
  }

  final class PostMessageImpl(
      memberRepository: MemberRepository,
      messageRepository: MessageRepository,
      getCurrentTimeMillis: GetCurrentTimeMillis
  ) extends PostMessage {
    import PostMessage._
    override def run(name: String, message: String): Try[PostMessage.Result] =
      memberRepository.resolveByName(name).flatMap {
        case Some(member) =>
          val newMessage = Message(
            id = messageRepository.nextId(),
            postBy = member.id,
            body = message,
            timestamp = getCurrentTimeMillis.run(),
            reader = Set.empty
          )
          messageRepository
            .store(newMessage)
            .map(_ => Result.Success)
        case None =>
          Success(Result.MemberNotFound)
      }
  }

  // ===

  abstract class FetchMessages {
    def run(name: String): Try[FetchMessages.Result]
  }
  object FetchMessages {
    abstract class Result extends Product with Serializable
    object Result {
      final case class Success(messages: Seq[MessageView]) extends Result
      case object MemberNotFound extends Result
    }

    final case class MessageView(name: String, message: String, readCount: Int)
  }

  final class FetchMessagesImpl(
      memberRepository: MemberRepository,
      messageRepository: MessageRepository,
      getCurrentTimeMillis: GetCurrentTimeMillis,
      getMemberName: GetMemberName
  ) extends FetchMessages {
    import FetchMessages._
    override def run(name: String): Try[FetchMessages.Result] =
      memberRepository.resolveByName(name).flatMap {
        case Some(member) =>
          for {
            messages <- messageRepository.resolveAllBy(
              timestamp = getCurrentTimeMillis.run(),
              limit = 10
            )
            view <- traverseTry(messages) { msg =>
              getMemberName.run(member.id).map { n =>
                MessageView(
                  name = n,
                  message = msg.body,
                  readCount = msg.readCount
                )
              }
            }
            _ <- traverseTry(messages) { msg =>
              messageRepository.store(msg.readBy(member.id))
            }
          } yield Result.Success(view)
        case None =>
          Success(Result.MemberNotFound)
      }
  }

  def traverseTry[A, B](xs: Seq[A])(f: A => Try[B]): Try[Seq[B]] =
    xs.foldLeft[Try[Seq[B]]](Success(Seq.empty)) { (acc, a) =>
      acc.flatMap(ys => f(a).map(ys :+ _))
    }

}
