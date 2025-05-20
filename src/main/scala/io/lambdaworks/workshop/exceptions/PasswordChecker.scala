package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {
    val checks = List(
      minNumberOfChars(password, 5),
      containsUpperCase(password),
      containsLowerCase(password),
      containsNumber(password)
    )

    val errors = checks.collect {
      case Left(err) => err
    }

    if (errors.nonEmpty) Left(errors)
    else Right(password)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] =
    if (password.length < length) Left(InvalidLength)
    else Right(password)

  private def containsUpperCase(password: String): Either[Throwable, String] =
    if (password.exists(_.isUpper)) Right(password)
    else Left(MissingUppercase)

  private def containsLowerCase(password: String): Either[Throwable, String] =
    if (password.exists(_.isLower)) Right(password)
    else Left(MissingLowercase)

  private def containsNumber(password: String): Either[Throwable, String] =
    if (password.exists(_.isDigit)) Right(password)
    else Left(MissingNumber)

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
