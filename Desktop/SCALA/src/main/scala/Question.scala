case class Question(name : String,
                    qType: String,
                    variants: Map[Int, String] = Map.empty,
                    results: Map[Int, Int] = Map.empty,
                    setOfNumbers : Iterator[Int] = Stream.from(1).iterator,
                    answers: Map[Int, String] = Map.empty) {
}