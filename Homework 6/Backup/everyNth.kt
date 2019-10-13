fun everyNth(L: List<Any>, N: Int): List<Any> {
    /* N is given to be a positive integer */
    val Nth: Int = N - 1
    val MAX: Int = L.size - 1
    /* L can be any type so we use a supertype */
    var Nth_list: MutableList<Any> = mutableListOf<Any>()
    /* for(i = N-1; i < L.size(); i += N) */
    for(index in Nth..MAX step N) {
        val element: Any = L.get(index)
        Nth_list.add(element)
    }

    /* Convert our MutableList to a regular List 
     * Kotlin's regular lists are readonly, hence immutable */
    val final_list: List<Any> = Nth_list.toList()
    return final_list
}

fun main() {
    println("Starting test...")
    val L: List<Int> = listOf(1,2,3,4,5,6,7,8,9,10)
    val N: Int = 2
    val A: List<Int> = listOf(2,4,6,8,10)
    val B: List<Any> = everyNth(L,N)
    if(B.equals(A)) {
        println("Test successful.")
    }
    else {
        println("Test failed.")
        println("The list L = " + L + " with N = " + N + " yielded:" )
        println(B)
    }
}