import example.Lists

object Main extends App {
	println(Lists.sum(List(1,3,2)))
	println(Lists.max(List(1,3,2)))
	println(Lists.max(List(1,2,3)))
	println(Lists.max(List(3,2,1)))
	println(Lists.max(List(3)))
	// println(Lists.max(List()))
}