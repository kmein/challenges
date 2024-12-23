import java.io.File

fun main() {
  val fileName = if (System.getenv("AOC_TEST") != null) { "23.txt.test" } else { "23.txt" }

  val graph = Graph()
  File(fileName).forEachLine { line ->
    val (source, destination) = line.split("-")
    graph.addEdge(source, destination)
  }

  val triangles = graph.findTriangles()
  println(triangles.count { triangle -> triangle.any { it.startsWith("t") } })

  val cliques = graph.findMaximalCliques()
  val password = cliques.maxByOrNull{it.size}?.toList()?.sorted()?.joinToString(",")
  println("Password: $password")
}

class Graph {
  private val adjacencyList: MutableMap<String, MutableList<String>> = mutableMapOf()

  fun addEdge(source: String, destination: String) {
    adjacencyList.computeIfAbsent(source) { mutableListOf() }.add(destination)
    adjacencyList.computeIfAbsent(destination) { mutableListOf() }.add(source) // For undirected graph
  }

  fun findMaximalCliques(): List<Set<String>> {
    val cliques = mutableListOf<Set<String>>()
    val vertices = adjacencyList.keys.toSet()
    bronKerbosch(emptySet(), vertices, emptySet(), cliques)
    return cliques
  }

  // havin' some private fun at the LAN party
  private fun bronKerbosch(
    R: Set<String>, // Current clique
    P: Set<String>, // Candidates
    X: Set<String>, // Excluded vertices
    cliques: MutableList<Set<String>>
  ) {
    if (P.isEmpty() && X.isEmpty()) {
      cliques.add(R)
      return
    }

    // pivot
    val u = (P + X).firstOrNull() ?: return

    var P = P
    var X = X
    for (v in P - (adjacencyList[u]?.toSet() ?: emptySet())) {
      val neighbors: Set<String> = adjacencyList[v]?.toSet() ?: emptySet()
      bronKerbosch(R + v, P.intersect(neighbors), X.intersect(neighbors), cliques)
      P = P - v
      X = X + v
    }
  }


  fun findTriangles(): Set<Set<String>> {
    val triangles = mutableSetOf<Set<String>>()

    // iterate over vertices
    for (node in adjacencyList.keys) {
      val neighbors = adjacencyList[node] ?: continue
      // check combinations of neighbors
      for (i in neighbors.indices) {
        for (j in i + 1 until neighbors.size) {
          val neighbor1 = neighbors[i]
          val neighbor2 = neighbors[j]
          // Check if neighbor1 and neighbor2 are connected
          if (adjacencyList[neighbor1]?.contains(neighbor2) == true) {
            triangles.add(setOf(node, neighbor1, neighbor2))
          }
        }
      }
    }

    return triangles
  }
}
