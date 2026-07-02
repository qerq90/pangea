# Pangea

Текстовая RPG для ВКонтакте на Scala + ZIO.

## Требования

- Docker и Docker Compose

## Деплой на VDS

### 1. Установить Docker

```bash
curl -fsSL https://get.docker.com | sh
```

### 2. Склонировать репозиторий

```bash
git clone <repo_url> /opt/pangea
cd /opt/pangea
```

### 3. Создать файл с секретами

```bash
cp .env.example .env
nano .env
```

Заполнить:

```env
POSTGRES_DB=pangea
POSTGRES_USER=pangea
POSTGRES_PASSWORD=your_strong_password

VK_TOKEN=your_vk_token_here
```

### 4. Запустить

```bash
docker compose up -d --build
```

При первом запуске Docker соберёт образ (~5 минут), поднимет PostgreSQL и прогонит все миграции автоматически.

### 5. Проверить

```bash
docker compose ps        # все сервисы должны быть Up
docker compose logs -f app  # логи бота
```

## Обновление

```bash
git pull
docker compose up -d --build
```

Миграции применяются автоматически при каждом запуске контейнера.

## Остановка

```bash
docker compose down        # остановить, данные сохранятся
docker compose down -v     # остановить и удалить БД
```

## Локальная разработка

Для запуска тестов PostgreSQL не нужен — тесты используют in-memory заглушки.

```bash
sbt core/test
```

Для локального запуска бота нужно поднять PostgreSQL (например, через `docker compose up postgres -d`) и задать переменные окружения:

```bash
export POSTGRES_JDBC_URL=jdbc:postgresql://localhost:5432/pangea
export POSTGRES_USER=pangea
export POSTGRES_PASSWORD=pangea
export VK_TOKEN=your_token
sbt app/run
```

## Архитектура

```
app/          — точка входа, HTTP-сервер (VK Callback API)
core/         — игровая логика, модели, состояния, DAO
migrations/   — SQL-миграции (goose)
```

Бот работает как конечный автомат: каждый игрок находится в одном из состояний (`Registration`, `Dungeon`, `HeroStats`, ...), которое хранится в БД. При получении события от VK вызывается обработчик текущего состояния.
