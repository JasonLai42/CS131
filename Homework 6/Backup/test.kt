import everyNth.kt

fun main() {
    println("Starting test...")
    val L: List<Int> = listOf(1,2,3,4,5,6,7,8,9,10)
    val N: Int = 2
    val A: List<Int> = listOf(2,4,6,8,10)
    val B: List<Any> = everyNth(L,N)
    if(B.equals(A)) {
        println("Success.")
    }
    else {
        println("Failure.")
        println("The list L " + L + " with N = " + N + "yielded:" )
        println(B)
    }

}