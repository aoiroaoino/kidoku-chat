package example

final class AppComponents {
  import model._
  import usecase._

  lazy val memberRepository: MemberRepository = new MemberRepositoryImpl
  lazy val messageRepository: MessageRepository = new MessageRepositoryImpl

  lazy val getCurrentTimeMillis: GetCurrentTimeMillis = new GetCurrentTimeMillisImpl
  lazy val getMemberName: GetMemberName = new GetMemberNameImpl(memberRepository)

  lazy val postMessage: PostMessage = new PostMessageImpl(memberRepository, messageRepository, getCurrentTimeMillis)
  lazy val fetchMessages: FetchMessages =
    new FetchMessagesImpl(memberRepository, messageRepository, getCurrentTimeMillis, getMemberName)

  lazy val consoleChat: ConsoleChat = new ConsoleChatImpl(postMessage, fetchMessages)
}
