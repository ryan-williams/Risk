object Risk {
	type Outcomes = Map[(Int, Int), Int]
	type OutcomesMap = Map[(Int, Int), (Int, Outcomes)]
	val kMaxNumAttackerDie = 3
	val kMaxNumDefenderDie = 2

	val singleRollOutcomeProbabilities = generateDiceProbabilities()
	def generateDiceProbabilities(): OutcomesMap = {
		(Map[(Int, Int), (Int, Outcomes)]() /: (for (numAttackerDie <- Range(1, kMaxNumAttackerDie + 1); numDefenderDie <- Range(1, kMaxNumDefenderDie + 1)) yield (numAttackerDie, numDefenderDie))) {
			(a, die) => {
				val frequencies = simulateDiceRoll(die._1, die._2)
				val total = frequencies.foldLeft(0)(_+_._2)
				a + (die -> (total, frequencies))
			}			
		}
	}

	def printSingleRollOutcomeProbabilities() = {
		for (outcome <- singleRollOutcomeProbabilities) {
			println("%d attacker die vs. %d defender die:".format(outcome._1._1, outcome._1._2))
			println(("" /: outcome._2._2) { (s, kv) => s + "\t%s: %d / %d = %1.3f\n".format(kv._1.toString, kv._2, outcome._2._1, kv._2 * 1.0 / outcome._2._1) })
		}		
	}

	def Usage = {
		println("Usage:")
		println("$ ./risk <attacking_force> <defending_force>")
		println("")
	}

	def main(args: Array[String]): Unit = {
		if (args.length < 2) {
			Usage
			return
		}
		try {
			args(0).toInt
			args(1).toInt
		} catch {
			case e: Exception => {
				Usage
				return
			}
		}
		val (numAttackerArmies, numDefenderArmies) = (args(0).toInt, args(1).toInt)
		val outcomes = getBattleProbabilityOutcomes(numAttackerArmies, numDefenderArmies).toList.sortWith(_._1>_._1)
		val (expectedAttackLoss, expectedDefenseLoss, probabilityOfAttackerVictory) =
		((0.0,0.0,0.0) /: (for (outcome <- outcomes) yield outcome)) {
			(avgs, outcome) => {
				if (outcome._1 > 0) (avgs._1 + ((numAttackerArmies - outcome._1) * outcome._2), avgs._2 + (numDefenderArmies * outcome._2), avgs._3 + outcome._2)
				else (avgs._1 + ((numAttackerArmies - 1) * outcome._2), avgs._2 + ((numDefenderArmies + outcome._1) * outcome._2), avgs._3)
			}
		}
		println("")
		println("When %d armies attack %d:" format(numAttackerArmies, numDefenderArmies))
		println("\n\tProbability of winning:\n\n\t\tAttacker: %1.3f\n\t\tDefender: %1.3f" format(probabilityOfAttackerVictory, 1.0 - probabilityOfAttackerVictory))
		println("\n\tExpected losses:\n\n\t\tAttacker: %1.3f\n\t\tDefender: %1.3f" format(expectedAttackLoss, expectedDefenseLoss))

		println("\n\tProbabilities of various outcomes:\n" format(numAttackerArmies, numDefenderArmies))
		val mode = (0.0 /: outcomes) {(max,outcome) => math.max(max, outcome._2)}
		var attackSection = true
		for (outcome <- outcomes) {
			if (outcome._1 <= 0 && attackSection) { println("\t\t" + ("-" * 100)); attackSection = false }
			println("\t\t%s wins by %3d: %1.3f | %s" format(
				if (outcome._1 > 0) "Attacker" else "Defender",
				math.abs(outcome._1), outcome._2,
				getHistogramString(outcome._2, mode)
			))
		}
		println("")
	}
	
	def getHistogramString(value: Double, max: Double): String = {
		val maxHistogramStringLength = 60
		val histogramStringChar = "x"
		histogramStringChar * (maxHistogramStringLength * value / max).toInt
	}
	
	var cachedProbabilities = Map[(Int, Int), Map[Int, Double]]()
	def getBattleProbabilityOutcomes(numAttackerArmies: Int, numDefenderArmies: Int): Map[Int, Double] = {
		if (cachedProbabilities.contains((numAttackerArmies, numDefenderArmies))) return cachedProbabilities((numAttackerArmies, numDefenderArmies))
		if (numAttackerArmies <= 1) return Map(-numDefenderArmies -> 1.0)
		if (numDefenderArmies <= 0) return Map(numAttackerArmies -> 1.0)
		val (total, possibleOutcomes) = singleRollOutcomeProbabilities((math.min(kMaxNumAttackerDie, numAttackerArmies - 1), math.min(kMaxNumDefenderDie, numDefenderArmies)))
		val allProbs: Iterable[Map[Int, Double]] = (for (outcome <- possibleOutcomes)
			yield {
				for (kv <- getBattleProbabilityOutcomes(numAttackerArmies - outcome._1._2, numDefenderArmies - outcome._1._1)) yield {
					(kv._1 -> kv._2 * outcome._2 * 1.0 / total)
				}
			}
		)
		mergeMaps[Int, Double](allProbs, (a,b)=>a+b)
	}
	
	def mergeMaps[A,B](ms: Iterable[Map[A,B]], f: (B,B)=>B): Map[A,B] = {
		(Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) {
			(a, kv) => a + (if (a.contains(kv._1)) (kv._1 -> f(a(kv._1), kv._2)) else kv)
		}
	}
	
	def simulateDiceRoll(numAttackerDie: Int, numDefenderDie: Int): Outcomes = simulateDiceRoll(Nil, numAttackerDie, Nil, numDefenderDie)
	def simulateDiceRoll(attackerDie: List[Int], totalNumAttackerDie: Int, defenderDie: List[Int], totalNumDefenderDie: Int): Outcomes = {
		if (attackerDie.length < totalNumAttackerDie) {
			mergeMaps(List(1,2,3,4,5,6).map(d => simulateDiceRoll(attackerDie ++ List(d), totalNumAttackerDie, defenderDie, totalNumDefenderDie)), (v1:Int, v2:Int) => v1 + v2)
		} else if (defenderDie.length < totalNumDefenderDie) {
			mergeMaps(List(1,2,3,4,5,6).map(d => simulateDiceRoll(attackerDie, totalNumAttackerDie, defenderDie ++ List(d), totalNumDefenderDie)), (v1:Int, v2:Int) => v1 + v2)
		} else {
			val score = scoreRolls(attackerDie, defenderDie)
			Map(score -> 1)
		}
	}
	
	def scoreRolls(attackerDie: List[Int], defenderDie: List[Int]): (Int, Int) = {
		val sortedAttackerDie = attackerDie.sortWith((a,b)=>(a>b)).slice(0, math.min(attackerDie.length, defenderDie.length))
		val sortedDefenderDie = defenderDie.sortWith((a,b)=>(a>b)).slice(0, math.min(attackerDie.length, defenderDie.length))
		val differentials = sortedAttackerDie.zip(sortedDefenderDie).map(p => p._1 - p._2)
		(differentials.count(_ > 0), differentials.count(_ <= 0))
	}
}