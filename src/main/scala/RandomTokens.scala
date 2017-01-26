/**
  * Created by paul on 26/01/2017.
  */
object RandomTokens {
  private var tmpTokenCount: Int = 0

  def getTmpToken: String = {
    tmpTokenCount += 1
    s"T_$tmpTokenCount"
  }

  def resetTmpToken(): Unit = tmpTokenCount = 0
}
