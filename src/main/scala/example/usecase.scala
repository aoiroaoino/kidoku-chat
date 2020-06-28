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

    override def run(): Unit = {
      println(WelcomeMessage)
      print(Prompt)
      Iterator
        .continually(StdIn.readLine())
        .takeWhile(!quit.matches(_))
        .map {
          case post(name, msg) =>
            postMessage.run(name, msg).map {
              case PostMessage.Result.Success        => PostSuccessMessage
              case PostMessage.Result.MemberNotFound => RequireMemberMessage
            }
          case fetch(name) =>
            fetchMessages.run(name).map {
              case FetchMessages.Result.Success(m) =>
                if (m.isEmpty) {
                  MessageNotFound
                } else {
                  val buf = new StringBuilder()
                  m.foreach {
                    case FetchMessages.MessageView(name, message, readCount) =>
                      buf.append("---\n")
                      buf.append(s"$name 「$message」")
                      buf.append(if (readCount == 0) "" else s"(既読 $readCount)")
                      buf.append("\n")
                  }
                  green(buf.toString)
                }
              case FetchMessages.Result.MemberNotFound => RequireMemberMessage
            }
          case input if input.isEmpty =>
            Success("")
          case input =>
            Success(InvalidInputMessage(input))
        }
        .foreach {
          case Success(v) if v.isEmpty =>
            print("\r" + Prompt)
          case Success(v) =>
            println(v)
            print(Prompt)
          case Failure(_) =>
            println(FailureMessage)
        }
    }
  }
  object ConsoleChatImpl {
    val quit = """:quit|:q""".r
    val post = """\s*(\S+)\s+post\s+([\S|\s]+)""".r
    val fetch = """\s*(\S+)\s+fetch\s*""".r

    def red(s: String): String = Console.RED + s + Console.RESET
    def green(s: String): String = Console.GREEN + s + Console.RESET
    def cyan(s: String): String = Console.CYAN + s + Console.RESET
    def yellow(s: String): String = Console.YELLOW + s + Console.RESET
    def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

    val WelcomeMessage = yellow("\nWelcome to serviver-challenge chat!!\n")
    val PostSuccessMessage = green("[送信に成功しました]\n")
    val MessageNotFound = green("[メッセージがありません]\n")
    val FailureMessage = red("[処理に失敗しました]\n")
    val RequireMemberMessage = red("[メンバー登録してください]\n")
    def InvalidInputMessage(input: String) = red(s"[不正な入力です: $input]\n")
    val Prompt = magenta("> ")
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
              getMemberName.run(msg.postBy).map { n =>
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
