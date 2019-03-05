import java.util.concurrent.ThreadLocalRandom


val v = Vector(0.1,0.4,0.5).scanLeft(0.0)(_ + _).tail
val p = ThreadLocalRandom.current().nextDouble()
v.indexWhere(a => p < a)