package example

import java.util.UUID

import scala.collection.mutable
import scala.util.{Success, Try}

object model {

  type EpochMillis = Long

  type MemberId = Int
  final case class Member(id: MemberId, name: String)

  abstract class MemberRepository {
    def resolveById(id: MemberId): Try[Option[Member]]
    def resolveByName(name: String): Try[Option[Member]]
  }
  final class MemberRepositoryImpl extends MemberRepository {
    val db = Seq(Member(1, "A"), Member(2, "B"), Member(3, "C"))
    override def resolveById(id: MemberId): Try[Option[Member]] =
      Success(db.find(_.id == id))
    override def resolveByName(name: String): Try[Option[Member]] =
      Success(db.find(_.name == name))
  }

  type MessageId = UUID
  final case class Message(
      id: MessageId,
      postBy: MemberId,
      body: String,
      reader: Set[MemberId],
      timestamp: EpochMillis
  ) {
    def readBy(memberId: MemberId): Message =
      copy(reader = reader + memberId)
    def readCount: Int = reader.count(_ != postBy)
  }

  abstract class MessageRepository {
    def nextId(): MessageId
    def store(message: Message): Try[Unit]
    def resolveAllBy(timestamp: EpochMillis, limit: Int): Try[Seq[Message]]
  }

  final class MessageRepositoryImpl extends MessageRepository {
    val db = mutable.ArrayBuffer.empty[Message]

    override def nextId: MessageId = UUID.randomUUID()

    override def store(message: Message): Try[Unit] = {
      db.indexWhere(_.id == message.id) match {
        case -1  => db += message
        case idx => db.update(idx, message)
      }
      Success(())
    }

    override def resolveAllBy(timestamp: EpochMillis, limit: MemberId): Try[Seq[Message]] =
      Success(db.sortBy(_.timestamp).takeWhile(_.timestamp < timestamp).takeRight(limit).toSeq)
  }
}
