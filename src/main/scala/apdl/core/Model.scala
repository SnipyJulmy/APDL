package apdl.core

object Model {
  private[apdl] var macAddress: Option[Seq[Byte]] = None
  private[apdl] var serverAddress: Option[Seq[Int]] = None
  private[apdl] var ipAddress: Option[Seq[Int]] = None
  private[apdl] var serverPort: Option[Int] = None
  private[apdl] var bufferSize: Option[Int] = None
}
