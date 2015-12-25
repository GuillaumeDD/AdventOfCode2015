package aoc.day21

import io.IO

object Part1 extends App {
  /*
--- Day 21: RPG Simulator 20XX ---

Little Henry Case got a new video game for Christmas. It's an RPG, 
and he's stuck on a boss. He needs to know what equipment to buy at 
the shop. He hands you the controller.

In this game, the player (you) and the enemy (the boss) take turns 
attacking. The player always goes first. Each attack reduces the 
opponent's hit points by at least 1. The first character at or below 
0 hit points loses.

Damage dealt by an attacker each turn is equal to the attacker's damage 
score minus the defender's armor score. An attacker always does at least 
1 damage. So, if the attacker has a damage score of 8, and the defender 
has an armor score of 3, the defender loses 5 hit points. If the defender 
had an armor score of 300, the defender would still lose 1 hit point.

Your damage score and armor score both start at zero. They can be increased 
by buying items in exchange for gold. You start with no items and have as 
much gold as you need. Your total damage or armor is equal to the sum of those 
stats from all of your items. You have 100 hit points.

Here is what the item shop is selling:

Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3

You must buy exactly one weapon; no dual-wielding. Armor is optional, 
but you can't use more than one. You can buy 0-2 rings (at most one for 
each hand). You must use any items you buy. The shop only has one of each 
item, so you can't buy, for example, two rings of Damage +3.

For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that 
the boss has 12 hit points, 7 damage, and 2 armor:

    The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

You have 100 hit points. The boss's actual stats are in your puzzle input. 
What is the least amount of gold you can spend and still win the fight?
  
 */
  // STUFF
  sealed abstract class Stuff {
    def name: String
    def cost: Int
    def damage: Int
    def armor: Int
  }
  case class Weapon(name: String, cost: Int, damage: Int, armor: Int) extends Stuff
  case class Armor(name: String, cost: Int, damage: Int, armor: Int) extends Stuff
  case class Ring(name: String, cost: Int, damage: Int, armor: Int) extends Stuff

  object Store {
    object Dagger extends Weapon("Dagger", 8, 4, 0)
    object Shortsword extends Weapon("Shortsword", 10, 5, 0)
    object Warhammer extends Weapon("Warhammer", 25, 6, 0)
    object Longsword extends Weapon("Longsword", 40, 7, 0)
    object Greataxe extends Weapon("Greataxe ", 74, 8, 0)
    val weapons = List(Dagger, Shortsword, Warhammer, Longsword, Greataxe)

    object Leather extends Armor("Leather", 13, 0, 1)
    object Chainmail extends Armor("Chainmail", 31, 0, 2)
    object Splintmail extends Armor("Splintmail", 53, 0, 3)
    object Bandedmail extends Armor("Bandedmail", 75, 0, 4)
    object Platemail extends Armor("latemail", 102, 0, 5)
    val armors = List(Leather, Chainmail, Splintmail, Bandedmail, Platemail)

    object Ring1 extends Ring("Damage +1", 25, 1, 0)
    object Ring2 extends Ring("Damage +2", 50, 2, 0)
    object Ring3 extends Ring("Damage +3", 100, 3, 0)
    object Ring4 extends Ring("Defense +1", 20, 0, 1)
    object Ring5 extends Ring("Defense +2", 40, 0, 2)
    object Ring6 extends Ring("Defense +3", 80, 0, 3)
    val rings = List(Ring1, Ring2, Ring3, Ring4, Ring5, Ring6)
  }

  case class StuffPackage(l: List[Stuff]) {
    def cost: Int = l.map(_.cost).sum
    def damage: Int = l.map(_.damage).sum
    def armor: Int = l.map(_.armor).sum
  }

  /*
   * You must buy exactly one weapon; no dual-wielding. 
   * Armor is optional, but you can't use more than one. 
   * You can buy 0-2 rings (at most one for each hand). 
   * You must use any items you buy. 
   * The shop only has one of each item, so you can't buy, 
   * for example, two rings of Damage +3.
   */
  val possibleStuffPackages =
    // Weapon only
    (for (w <- Store.weapons) yield StuffPackage(List(w))) ++
      // Weapon and armor
      (for {
        w <- Store.weapons
        armor <- Store.armors
      } yield StuffPackage(List(w, armor))) ++
      // Weapon, armor and 1 ring
      (for {
        w <- Store.weapons
        armor <- Store.armors
        ring <- Store.rings
      } yield StuffPackage(List(w, armor, ring))) ++
      // Weapon, armor and 2 ring
      (for {
        w <- Store.weapons
        armor <- Store.armors
        ring1 <- Store.rings
        ring2 <- Store.rings
        if ring1 != ring2
      } yield StuffPackage(List(w, armor, ring1, ring2)))

  // CHARACTER
  abstract class Character {
    def hitPoint: Int
    def damage: Int
    def armor: Int
    def decreaseHitPoint(v: Int): Unit
    def isAlive: Boolean =
      hitPoint > 0
  }

  case class Player(var hitPoint: Int, p: StuffPackage) extends Character {
    def damage: Int =
      p.damage
    def armor: Int =
      p.armor
    def decreaseHitPoint(v: Int): Unit =
      hitPoint -= v
  }

  // Input
  val bossStats =
    """Hit Points: ([0-9]+)
Damage: ([0-9]+)
Armor: ([0-9]+)""".r
  val input = IO.getLines().mkString("\n")
  val (hPoint, damagePoint, armorPoint) =
    input match {
      case bossStats(h, d, a) => (h.toInt, d.toInt, a.toInt)
    }

  object Boss extends Character {
    var hitPoint = hPoint
    def damage = damagePoint
    def armor = armorPoint
    def decreaseHitPoint(v: Int): Unit =
      hitPoint -= v

    def reset(): Unit = {
      hitPoint = hPoint
    }
  }

  // GAME
  case class Game(p: Player) {
    def playerWin(): Boolean = {
      Boss.reset()
      var isPlayerTurn = true
      while (p.isAlive && Boss.isAlive) {
        if (isPlayerTurn) {
          playerTurn()
          isPlayerTurn = false
        } else {
          bossTurn()
          isPlayerTurn = true
        }
      }

      p.isAlive
    }

    private def playerTurn(): Unit = {
      // An attacker always does at least 1 damage.
      val damage = Math.max(p.damage - Boss.armor, 1)

      Boss.decreaseHitPoint(damage)
    }
    private def bossTurn(): Unit = {
      // An attacker always does at least 1 damage.
      val damage = Math.max(Boss.damage - p.armor, 1)

      p.decreaseHitPoint(damage)
    }
  }

  val winingPackage =
    possibleStuffPackages
      .filter(pack => Game(Player(100, pack)).playerWin())
      .minBy(_.cost)

  println(s"The least amount of gold you can spend and still win the fight is:\n$winingPackage, cost: ${winingPackage.cost}")

  /*
     --- Part Two ---

Turns out the shopkeeper is working with the boss, and can 
persuade you to buy whatever items he wants. The other rules 
still apply, and he still only has one of each item.

What is the most amount of gold you can spend and still lose the fight?
 */
  val loosingPackage =
    possibleStuffPackages
      .filter(pack => !Game(Player(100, pack)).playerWin())
      .maxBy(_.cost)

  println(s"The most amount of gold you can spend and still lose the fight is:\n$loosingPackage, cost: ${loosingPackage.cost}")
}