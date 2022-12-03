object ElfRockPaperScissorsJob extends App {
  val data = ElfRockPaperScissors.openFile("/Users/curnowl/Developer/AdventOfCode2022/day_2_RPS/src/main/scala/RPS_input.txt")
  println(ElfRockPaperScissors.roundTotaller(data))
}
