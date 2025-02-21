import csv.*

case class User(
    @csvLabel("custom_name")
    name: String,
    isActive: Boolean,
    emailAddress: String
) derives CsvRowEncoder


@main
def main() =
  val user = User("Kawkowski", true, "aqfq@aqq.pl")

  println(user.csvHeader)
  println(user.asCsvLine)

  assert(user.csvHeader == "custom_name,is_active,email_address")
  assert(user.asCsvLine == "Kawkowski,true,aqfq@aqq.pl")
