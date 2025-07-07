
import scala.io.StdIn
import scala.util.Random

// Базовые характеристики персонажа
case class Stats(intelligence: Int, strength: Int, constitution: Int, agility: Int)

// Боевые характеристики
case class CombatStats(hp: Int, maxHp: Int, attack: Int, armor: Int, defence: Int)

// Типы экипировки
sealed trait EquipmentType
case object Weapon extends EquipmentType
case object Armor extends EquipmentType
case object Helmet extends EquipmentType
case object Boots extends EquipmentType

// Экипировка
case class Equipment(
  name: String,
  equipmentType: EquipmentType,
  attackBonus: Int = 0,
  armorBonus: Int = 0,
  defenceBonus: Int = 0
)

// Инвентарь и экипированные предметы
case class Inventory(
  items: List[Equipment] = List.empty,
  equipped: Map[EquipmentType, Equipment] = Map.empty
)

// Персонаж игрока
case class Player(
  name: String,
  stats: Stats,
  combatStats: CombatStats,
  inventory: Inventory,
  level: Int = 1
) {
  def calculateCombatStats(): CombatStats = {
    val baseHp = 100 + stats.constitution * 10
    val baseAttack = 10 + stats.strength * 2
    val baseArmor = stats.constitution
    val baseDefence = 5 + stats.agility

    val equipmentAttackBonus = inventory.equipped.values.map(_.attackBonus).sum
    val equipmentArmorBonus = inventory.equipped.values.map(_.armorBonus).sum
    val equipmentDefenceBonus = inventory.equipped.values.map(_.defenceBonus).sum

    CombatStats(
      hp = math.min(combatStats.hp, baseHp),
      maxHp = baseHp,
      attack = baseAttack + equipmentAttackBonus,
      armor = baseArmor + equipmentArmorBonus,
      defence = baseDefence + equipmentDefenceBonus
    )
  }

  def equipItem(equipment: Equipment): Player = {
    val newEquipped = inventory.equipped + (equipment.equipmentType -> equipment)
    val newItems = inventory.items.filterNot(_ == equipment)
    val newInventory = inventory.copy(items = newItems, equipped = newEquipped)
    val updatedPlayer = this.copy(inventory = newInventory)
    updatedPlayer.copy(combatStats = updatedPlayer.calculateCombatStats())
  }

  def unequipItem(equipmentType: EquipmentType): Player = {
    inventory.equipped.get(equipmentType) match {
      case Some(equipment) =>
        val newEquipped = inventory.equipped - equipmentType
        val newItems = equipment :: inventory.items
        val newInventory = inventory.copy(items = newItems, equipped = newEquipped)
        val updatedPlayer = this.copy(inventory = newInventory)
        updatedPlayer.copy(combatStats = updatedPlayer.calculateCombatStats())
      case None => this
    }
  }

  def addItem(equipment: Equipment): Player = {
    val newInventory = inventory.copy(items = equipment :: inventory.items)
    this.copy(inventory = newInventory)
  }

  def takeDamage(damage: Int): Player = {
    val actualDamage = math.max(1, damage - combatStats.armor)
    val newHp = math.max(0, combatStats.hp - actualDamage)
    this.copy(combatStats = combatStats.copy(hp = newHp))
  }

  def isAlive: Boolean = combatStats.hp > 0
}

// Монстр
case class Monster(
  name: String,
  hp: Int,
  maxHp: Int,
  attack: Int,
  defence: Int,
  lootTable: List[Equipment] = List.empty
) {
  def takeDamage(damage: Int): Monster = {
    val actualDamage = math.max(1, damage - defence)
    val newHp = math.max(0, hp - actualDamage)
    this.copy(hp = newHp)
  }

  def isAlive: Boolean = hp > 0
}

// События в подземелье
sealed trait DungeonEvent
case class MonsterEncounter(monster: Monster) extends DungeonEvent
case class TreasureFound(equipment: Equipment) extends DungeonEvent
case object EmptyRoom extends DungeonEvent
case object RestArea extends DungeonEvent

// Действия в бою
sealed trait CombatAction
case object Attack extends CombatAction
case object Defend extends CombatAction
case object Flee extends CombatAction

// Результат боя
sealed trait CombatResult
case object Victory extends CombatResult
case object Defeat extends CombatResult
case object Escaped extends CombatResult

// Игровой движок
class GameEngine {
  val random = new Random()

  // Предопределенная экипировка
  val equipmentPool = List(
    Equipment("Ржавый меч", Weapon, attackBonus = 5),
    Equipment("Железный меч", Weapon, attackBonus = 10),
    Equipment("Стальной меч", Weapon, attackBonus = 15),
    Equipment("Кожаная броня", Armor, armorBonus = 3),
    Equipment("Железная броня", Armor, armorBonus = 6),
    Equipment("Стальная броня", Armor, armorBonus = 10),
    Equipment("Кожаный шлем", Helmet, defenceBonus = 2),
    Equipment("Железный шлем", Helmet, defenceBonus = 4),
    Equipment("Кожаные сапоги", Boots, defenceBonus = 1, armorBonus = 1),
    Equipment("Железные сапоги", Boots, defenceBonus = 2, armorBonus = 2)
  )

  // Пул монстров
  def createMonster(): Monster = {
    val monsterTypes = List(
      Monster("Гоблин", 30, 30, 8, 1, equipmentPool.take(3)),
      Monster("Орк", 50, 50, 12, 3, equipmentPool.drop(2).take(4)),
      Monster("Скелет", 40, 40, 10, 2, equipmentPool.drop(1).take(3)),
      Monster("Огр", 80, 80, 15, 5, equipmentPool.drop(3))
    )
    monsterTypes(random.nextInt(monsterTypes.length))
  }

  // Генерация событий
  def generateEvent(): DungeonEvent = {
    random.nextInt(100) match {
      case x if x < 40 => MonsterEncounter(createMonster())
      case x if x < 60 => TreasureFound(equipmentPool(random.nextInt(equipmentPool.length)))
      case x if x < 80 => EmptyRoom
      case _ => RestArea
    }
  }

  // Утилитарная функция для безопасного ввода
  def safeReadInput(prompt: String = ""): String = {
    if (prompt.nonEmpty) print(prompt)
    // Принудительно очищаем буфер
    System.out.flush()
    try {
      val input = StdIn.readLine()
      if (input != null) {
        input.trim
      } else {
        ""
      }
    } catch {
      case _: Exception => ""
    }
  }

  // Обработка боя
  def processCombat(player: Player, monster: Monster): (Player, Monster, CombatResult) = {
    println(s"\n=== БОЙ С ${monster.name.toUpperCase} ===")
    println(s"${monster.name}: ${monster.hp}/${monster.maxHp} HP")
    println(s"${player.name}: ${player.combatStats.hp}/${player.combatStats.maxHp} HP")

    var currentPlayer = player
    var currentMonster = monster

    var combatRounds = 0
    val maxRounds = 50 // Защита от бесконечного цикла
    
    while (currentPlayer.isAlive && currentMonster.isAlive && combatRounds < maxRounds) {
      combatRounds += 1
      println(s"\n--- Ход ${combatRounds} ---")
      println("Выберите действие:")
      println("1. Атаковать")
      println("2. Защищаться") 
      println("3. Сбежать")

      var actionTaken = false
      var playerAction: CombatAction = Attack
      
      // Упрощенный ввод без циклов
      val choice = safeReadInput("Ваш выбор (1-3): ")
      choice match {
        case "1" => 
          playerAction = Attack
          actionTaken = true
        case "2" => 
          playerAction = Defend
          actionTaken = true
        case "3" => 
          playerAction = Flee
          actionTaken = true
        case _ => 
          println("Автоматически выбрана атака!")
          playerAction = Attack
          actionTaken = true
      }

      if (actionTaken) {
        playerAction match {
          case Flee =>
            if (random.nextInt(100) < 30 + currentPlayer.stats.agility) {
              println("Вы успешно сбежали!")
              return (currentPlayer, currentMonster, Escaped)
            } else {
              println("Побег не удался!")
            }

          case Attack =>
            val damage = currentPlayer.combatStats.attack + random.nextInt(5)
            val actualDamage = math.max(1, damage - currentMonster.defence)
            currentMonster = currentMonster.takeDamage(damage)
            println(s"Вы нанесли ${actualDamage} урона!")

          case Defend =>
            println("Вы готовитесь к защите!")
        }

        // Проверяем, жив ли монстр после атаки игрока
        if (!currentMonster.isAlive) {
          println(s"${currentMonster.name} повержен!")
        } else {
          // Атака монстра
          val monsterDamage = currentMonster.attack + random.nextInt(3)
          val reducedDamage = if (playerAction == Defend) monsterDamage / 2 else monsterDamage
          val actualDamage = math.max(1, reducedDamage - currentPlayer.combatStats.armor)
          currentPlayer = currentPlayer.takeDamage(reducedDamage)
          println(s"${currentMonster.name} нанес вам ${actualDamage} урона!")
          
          println(s"${currentMonster.name}: ${currentMonster.hp}/${currentMonster.maxHp} HP")
          println(s"${currentPlayer.name}: ${currentPlayer.combatStats.hp}/${currentPlayer.combatStats.maxHp} HP")
        }
      }
    }

    if (!currentPlayer.isAlive) {
      (currentPlayer, currentMonster, Defeat)
    } else {
      (currentPlayer, currentMonster, Victory)
    }
  }

  // Управление инвентарем
  def manageInventory(player: Player): Player = {
    var currentPlayer = player
    var managing = true

    while (managing) {
      println("\n=== ИНВЕНТАРЬ ===")
      println("Экипированные предметы:")
      currentPlayer.inventory.equipped.foreach { case (equipType, equipment) =>
        println(s"  ${equipType}: ${equipment.name}")
      }

      println("\nПредметы в сумке:")
      currentPlayer.inventory.items.zipWithIndex.foreach { case (equipment, index) =>
        println(s"  ${index + 1}. ${equipment.name} (${equipment.equipmentType})")
      }

      println("\nВыберите действие:")
      println("1. Экипировать предмет")
      println("2. Снять экипировку")
      println("3. Выйти из инвентаря")

      val inventoryChoice = safeReadInput("Ваш выбор: ")
      inventoryChoice match {
        case "1" =>
          if (currentPlayer.inventory.items.nonEmpty) {
            val indexInput = safeReadInput("Введите номер предмета для экипировки: ")
            try {
              val index = indexInput.toInt - 1
              if (index >= 0 && index < currentPlayer.inventory.items.length) {
                val equipment = currentPlayer.inventory.items(index)
                currentPlayer = currentPlayer.equipItem(equipment)
                println(s"Экипирован: ${equipment.name}")
              } else {
                println("Неверный номер предмета!")
              }
            } catch {
              case _: NumberFormatException => println("Неверный номер!")
            }
          } else {
            println("У вас нет предметов для экипировки!")
          }

        case "2" =>
          println("Выберите тип экипировки для снятия:")
          println("1. Оружие")
          println("2. Броня")
          println("3. Шлем")
          println("4. Сапоги")
          
          val equipChoice = safeReadInput("Ваш выбор: ")
          equipChoice match {
            case "1" => 
              currentPlayer = currentPlayer.unequipItem(Weapon)
              println("Оружие снято!")
            case "2" => 
              currentPlayer = currentPlayer.unequipItem(Armor)
              println("Броня снята!")
            case "3" => 
              currentPlayer = currentPlayer.unequipItem(Helmet)
              println("Шлем снят!")
            case "4" => 
              currentPlayer = currentPlayer.unequipItem(Boots)
              println("Сапоги сняты!")
            case _ => println("Неверный выбор!")
          }

        case "3" => managing = false
        case _ => println("Неверный выбор!")
      }
    }
    currentPlayer
  }
}

// Основной объект игры
object Main {
  def main(args: Array[String]): Unit = {
    val game = new GameEngine()
    
    println("=== ДОБРО ПОЖАЛОВАТЬ В ПОДЗЕМЕЛЬЕ ===")
    val playerName = game.safeReadInput("Введите имя вашего персонажа: ")

    // Создание персонажа
    val initialStats = Stats(
      intelligence = 10 + game.random.nextInt(6),
      strength = 10 + game.random.nextInt(6),
      constitution = 10 + game.random.nextInt(6),
      agility = 10 + game.random.nextInt(6)
    )

    val startingEquipment = Equipment("Деревянная дубинка", Weapon, attackBonus = 2)
    val inventory = Inventory(
      items = List.empty,
      equipped = Map(Weapon -> startingEquipment)
    )

    var player = Player(playerName, initialStats, CombatStats(0, 0, 0, 0, 0), inventory)
    player = player.copy(combatStats = player.calculateCombatStats())
    // Установить полное HP при создании персонажа
    player = player.copy(combatStats = player.combatStats.copy(hp = player.combatStats.maxHp))

    println(s"\nПерсонаж создан: ${player.name}")
    println(s"Характеристики:")
    println(s"  Интеллект: ${player.stats.intelligence}")
    println(s"  Сила: ${player.stats.strength}")
    println(s"  Телосложение: ${player.stats.constitution}")
    println(s"  Ловкость: ${player.stats.agility}")

    // Основной игровой цикл
    var gameRunning = true
    while (gameRunning && player.isAlive) {
      println("\n" + "="*50)
      println(s"${player.name} - HP: ${player.combatStats.hp}/${player.combatStats.maxHp}")
      println("Что вы хотите сделать?")
      println("1. Исследовать подземелье")
      println("2. Управлять инвентарем")
      println("3. Посмотреть характеристики")
      println("4. Выйти из игры")

      val mainChoice = game.safeReadInput("Ваш выбор: ")
      mainChoice match {
        case "1" =>
          // Исследование подземелья
          val event = game.generateEvent()
          event match {
            case MonsterEncounter(monster) =>
              val (updatedPlayer, _, result) = game.processCombat(player, monster)
              player = updatedPlayer
              
              result match {
                case Victory =>
                  println("Победа! Монстр повержен!")
                  // Шанс на лут
                  if (game.random.nextInt(100) < 50 && monster.lootTable.nonEmpty) {
                    val loot = monster.lootTable(game.random.nextInt(monster.lootTable.length))
                    player = player.addItem(loot)
                    println(s"Вы нашли: ${loot.name}")
                  }
                case Defeat =>
                  println("Поражение! Вы погибли...")
                  gameRunning = false
                case Escaped =>
                  // Уже обработано в бою
              }

            case TreasureFound(equipment) =>
              println(s"Вы нашли сокровище: ${equipment.name}!")
              player = player.addItem(equipment)

            case EmptyRoom =>
              println("Пустая комната. Ничего интересного.")

            case RestArea =>
              println("Вы нашли место для отдыха!")
              val healAmount = player.combatStats.maxHp / 4
              val oldHp = player.combatStats.hp
              val newHp = math.min(player.combatStats.maxHp, player.combatStats.hp + healAmount)
              player = player.copy(combatStats = player.combatStats.copy(hp = newHp))
              println(s"Восстановлено ${newHp - oldHp} HP")
          }

        case "2" =>
          player = game.manageInventory(player)

        case "3" =>
          println(s"\n=== ХАРАКТЕРИСТИКИ ${player.name.toUpperCase} ===")
          println(s"Уровень: ${player.level}")
          println("\nОсновные характеристики:")
          println(s"  Интеллект: ${player.stats.intelligence}")
          println(s"  Сила: ${player.stats.strength}")
          println(s"  Телосложение: ${player.stats.constitution}")
          println(s"  Ловкость: ${player.stats.agility}")
          println("\nБоевые характеристики:")
          println(s"  HP: ${player.combatStats.hp}/${player.combatStats.maxHp}")
          println(s"  Атака: ${player.combatStats.attack}")
          println(s"  Броня: ${player.combatStats.armor}")
          println(s"  Защита: ${player.combatStats.defence}")

        case "4" =>
          println("Спасибо за игру!")
          gameRunning = false

        case _ =>
          println("Неверный выбор!")
      }
    }

    if (!player.isAlive) {
      println("\n=== ИГРА ОКОНЧЕНА ===")
      println("Ваш персонаж погиб в подземелье...")
    }
  }
}
