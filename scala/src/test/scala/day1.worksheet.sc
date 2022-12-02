val ex = """
2739
2663
2678
1627
7779
7845
9191
9656
"""

val input = ex.trim.split("\n").map(_.trim.toInt).toList

input.sum
