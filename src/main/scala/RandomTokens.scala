/**
  * Created by paul on 26/01/2017.
  *
  * RandomTokens.
  *
  * Generate random temporary tokens (as temporary variable names) of form `T_<int>`.
  */
object RandomTokens {
  private var tmpTokenCount: Int = 0

  /**
    * Get a temporary token.
    *
    * @return the token string.
    */
  def getTmpToken: String = {
    tmpTokenCount += 1
    s"T_$tmpTokenCount"
  }

  /**
    * Reset counter to zero.
    *
    * NOTE call this when a new solving starts. NEVER call it when a solving is still running!
    */
  def resetTmpToken(): Unit = tmpTokenCount = 0
}
