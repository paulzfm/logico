/**
  * Created by paul on 29/01/2017.
  */

import Types._

abstract class RuntimeError extends Exception {
  def msg: String

  override def getMessage: String = s"Runtime ERROR: $msg"
}

class NoSuchSignatureError(sig: Sig) extends RuntimeError {
  override def msg: String = s"no such signature in database: $sig."
}

class NoSuchCommandError(op: String) extends RuntimeError {
  override def msg: String = s"no such command: $op."
}

class MissingArgumentsError(op: String, expected: Int) extends RuntimeError {
  override def msg: String = s"missing argument for $op: $expected expected."
}

class InvalidArgumentsError(arg: String, expected: String) extends RuntimeError {
  override def msg: String = s"invalid argument: $arg, $expected expected."
}