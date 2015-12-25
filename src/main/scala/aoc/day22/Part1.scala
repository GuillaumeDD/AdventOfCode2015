package aoc.day22

import scala.annotation.tailrec
import io.IO

object Part1 extends App {
  /*
--- Day 22: Wizard Simulator 20XX ---

Little Henry Case decides that defeating bosses with swords 
and stuff is boring. Now he's playing the game with a wizard. 
Of course, he gets stuck on another boss and needs your help 
again.

In this version, combat still proceeds with the player and 
the boss taking alternating turns. The player still goes 
first. Now, however, you don't get any equipment; instead, 
you must choose one of your spells to cast. The first 
character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you 
can't attack normally. However, since you do magic damage, 
your opponent's armor is ignored, and so the boss effectively 
has zero armor as well. As before, if armor (from a spell, in 
this case) would reduce damage below 1, it becomes 1 instead 
- that is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to 
cast. If you cannot afford to cast any spell, you lose. Spells 
cost mana; you start with 500 mana, but have no maximum limit. 
You must have enough mana to cast a spell, and its cost is 
immediately deducted when you cast it. Your spells are Magic 
Missile, Drain, Shield, Poison, and Recharge.

    Magic Missile costs 53 mana. It instantly does 4 damage.
    Drain costs 73 mana. It instantly does 2 damage and heals 
    you for 2 hit points.
    Shield costs 113 mana. It starts an effect that lasts for 
    6 turns. While it is active, your armor is increased by 7.
    Poison costs 173 mana. It starts an effect that lasts for 6 
    turns. At the start of each turn while it is active, it deals 
    the boss 3 damage.
    Recharge costs 229 mana. It starts an effect that lasts for 
    5 turns. At the start of each turn while it is active, it gives 
    you 101 new mana.

Effects all work the same way. Effects apply at the start of both 
the player's turns and the boss' turns. Effects are created with 
a timer (the number of turns they last); at the start of each turn,
 after they apply any effect they have, their timer is decreased by
  one. If this decreases the timer to zero, the effect ends. You 
  cannot cast a spell that would start an effect which is already 
  active. However, effects can be started on the same turn they end.

For example, suppose the player has 10 hit points and 250 mana, and 
that the boss has 13 hit points and 8 damage:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.

-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.

-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player 
wins.

Now, suppose the same initial conditions, except that the boss 
has 14 hit points instead:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.

-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!

-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.

-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.

-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.

-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points. The boss's actual
 stats are in your puzzle input. What is the least amount of mana 
 you can spend and still win the fight? (Do not include mana recharge 
 effects as "spending" negative mana.)
   */
  // EFFECTS AND SPELLS
  case class Effect(
      val cast: Cast,
      val timer: Int) {
    def tic(): Effect =
      copy(timer = timer - 1)

    def isEffective(): Boolean =
      timer > 0
  }

  sealed abstract class AbstractSpell {
    def cost(): Int
    def name(): String
  }

  case class Spell(
    val name: String,
    val cost: Int,
    val damage: Int,
    val heal: Int) extends AbstractSpell

  object MagicMissile extends Spell("MagicMissile", 53, 4, 0)
  object Drain extends Spell("Drain", 73, 2, 2)

  case class Cast(
      val name: String,
      val cost: Int,
      val duration: Int,
      val affectPlayer: Boolean = true) extends AbstractSpell {
    def buildEffect(): Effect = Effect(this, duration)
  }

  object Shield extends Cast("Shield", 113, 6)
  object Poison extends Cast("Poison", 173, 6, false)
  object Recharge extends Cast("Recharge", 229, 5)

  val spells = List[AbstractSpell](MagicMissile, Drain, Shield, Poison, Recharge)

  // CHARACTERS
  abstract class Character {
    def hitPoint: Int

    def decreaseHitPoint(v: Int): Character
    def isAlive: Boolean =
      hitPoint > 0

    def isDead: Boolean = !isAlive
  }

  case class Player(
      val hitPoint: Int = 50,
      val mana: Int = 500,
      val armor: Int = 0) extends Character {
    def decreaseHitPoint(v: Int): Player =
      copy(hitPoint = hitPoint - v)

    def increaseArmor(v: Int): Player =
      copy(armor = armor + v)
    def resetArmor(): Player =
      copy(armor = 0)

    def increaseMana(v: Int): Player =
      copy(mana = mana + v)
    def decreaseMana(v: Int): Player =
      copy(mana = mana - v)

    def heal(v: Int): Player =
      copy(hitPoint = hitPoint + v)
  }

  // Input
  val bossStats =
    """Hit Points: ([0-9]+)
Damage: ([0-9]+)""".r
  val input = IO.getLines().mkString("\n")
  val (hPoint, damagePoint) =
    input match {
      case bossStats(h, d) => (h.toInt, d.toInt)
    }

  case class Boss(val hitPoint: Int = hPoint) extends Character {
    def damage = damagePoint

    def decreaseHitPoint(v: Int): Boss =
      Boss(hitPoint - v)
  }

  // GAME STATE
  case class GameState(
      val player: Player,
      val boss: Boss,
      val isPlayerTurn: Boolean = true, // The player still goes first
      val isPlayerBlocked: Boolean = false, // If you cannot afford to cast any spell, you lose.
      val manaCost: Int = 0,
      val effects: List[Effect] = List(),
      val hard: Boolean = false) {
    def isActive(spell: AbstractSpell): Boolean =
      effects.exists(_.cast == spell)

    def isInactive(spell: AbstractSpell): Boolean =
      !isActive(spell)

    def isFinished(): Boolean =
      player.isDead || boss.isDead || isPlayerBlocked

    def hasPlayerWon(): Boolean =
      boss.isDead

    // MANAGEMENT OF EFFECTS
    private def applyEffect(e: Effect): GameState =
      e.cast match {
        case Shield =>
          copy(player = player.increaseArmor(7))
        case Poison =>
          copy(boss = boss.decreaseHitPoint(3))
        case Recharge =>
          copy(player = player.increaseMana(101))
      }

    private def clearEffects(): GameState = {
      copy(effects = effects.map(_.tic()).filter(_.isEffective()))
    }

    private def applyEffects(): GameState = {
      // Effects apply at the start of both the player's turns and the boss' turns.
      // at the start of each turn, after they apply any effect they have, 
      // their timer is decreased by one.

      effects.foldLeft(this) {
        (newGameState, effect) => newGameState.applyEffect(effect)
      } // If this decreases the timer to zero, the effect ends.
        .clearEffects()
    }

    private def resetTemporaryEffects(): GameState = {
      copy(player = player.resetArmor())
    }

    private def startEffect(cast: Cast): GameState = {
      copy(player = player.decreaseMana(cast.cost),
        effects = Effect(cast, cast.duration) :: effects)
    }

    // MANAGEMENT OF MANA
    private def addCost(cost: Int): GameState = {
      copy(manaCost = manaCost + cost)
    }

    private def playerHasNotEnoughMana(): GameState = {
      copy(isPlayerBlocked = true)
    }

    // MANAGEMENT OF SPELL
    private def applySpell(spell: AbstractSpell): GameState = {
      ((spell: @unchecked) match {
        case MagicMissile =>
          copy(player = player.decreaseMana(MagicMissile.cost),
            boss = boss.decreaseHitPoint(4) // It instantly does 4 damage.
            )
        case Drain =>
          copy(player = player.decreaseMana(Drain.cost)
            // heals you for 2 hit points.
            .heal(2),
            boss = boss.decreaseHitPoint(2) // It instantly does 2 damage
            )
        case Shield =>
          startEffect(Shield)
        case Poison =>
          startEffect(Poison)
        case Recharge =>
          startEffect(Recharge)
      }).addCost(spell.cost)
    }

    private def nextTurn(): GameState = {
      copy(isPlayerTurn = !isPlayerTurn)
    }

    private def computeTurn(): List[GameState] = {
      if (isFinished()) {
        // BOSS OR PLAYER KILLED BY EFFECTS
        List(this)
      } else {
        if (isPlayerTurn) {
          // PLAYER TURN
          val possibleSpells =
            spells
              // You cannot cast a spell that  would start an effect which is already active.
              .filter(isInactive(_))
              .filter(_.cost() <= player.mana)

          if (possibleSpells.isEmpty) {
            // If you cannot afford to cast any spell, you lose.
            List(playerHasNotEnoughMana())
          } else {
            for {
              move <- possibleSpells
            } yield (applySpell(move).nextTurn())
          }

        } else {
          // BOSS TURN
          // the boss' attacks always deal at least 1 damage.
          val bossAttack = Math.max(boss.damage - player.armor, 1)
          List(copy(
            player = player.decreaseHitPoint(bossAttack),
            isPlayerTurn = !isPlayerTurn))
        }
      }
    }

    def standardMode(): List[GameState] = {
      if (!isFinished()) {
        // APPLICATION OF EFFECTS
        this.resetTemporaryEffects()
          //  Effects apply at the start of both the player's turns and the boss' turns.
          .applyEffects()
          .computeTurn()

      } else {
        // The game is finished, the state stay the same
        List(this)
      }

    }

    def next(): List[GameState] = {
      if (hard && isPlayerTurn) {
        this.copy(player = player.decreaseHitPoint(1))
          .standardMode()
      } else {
        standardMode()
      }
    }
  }

  @tailrec
  def exploreAllPossibleGames(
    currentGames: List[GameState] = List(GameState(Player(), Boss()))): List[GameState] = {
    if (currentGames.exists(!_.isFinished())) {
      // Pruning if possible
      val bestGames = currentGames.filter(_.hasPlayerWon())
      bestGames match {
        case List() =>
          exploreAllPossibleGames(currentGames.flatMap(_.next()))
        case other =>
          val bestGameCost = bestGames.minBy(_.manaCost).manaCost
          val newCurrentGames = currentGames.filter(_.manaCost <= bestGameCost)
          exploreAllPossibleGames(newCurrentGames.flatMap(_.next()))
      }
    } else {
      currentGames
    }

  }

  val allWiningGames = exploreAllPossibleGames().filter(_.hasPlayerWon())
  val bestGame = allWiningGames.minBy(_.manaCost)
  println(s"The least amount of mana you can spend and still win the fight is: ${bestGame.manaCost}")

  /*
--- Part Two ---

On the next run through the game, you increase the difficulty to hard.

At the start of each player turn (before any other effects apply), you 
lose 1 hit point. If this brings you to or below 0 hit points, you lose.

With the same starting stats for you and the boss, what is the least 
amount of mana you can spend and still win the fight?

 */
  val allWiningGames2 = exploreAllPossibleGames(List(GameState(Player(), Boss(), hard = true))).filter(_.hasPlayerWon())
  val bestGame2 = allWiningGames2.minBy(_.manaCost)
  println(s"The least amount of mana you can spend and still win the fight: ${bestGame2.manaCost}")
}