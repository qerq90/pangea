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

# Пароль админ-панели («/admin» в личке бота) и выдачи логов по HTTP.
# Пусто — и панель, и `GET /logs` закрыты для всех.
ADMIN_PASSWORD=
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

## Донат (покупка дублонов)

Продажа дублонов за рубли через интернет-эквайринг Т-Банка. Вход — Торговый дом
на Торговой площади, банкир Рахадим → «Купить дублоны».

Пока `TBANK_TERMINAL_KEY` и `TBANK_PASSWORD` пусты, донат выключен: экран у
Рахадима остаётся заглушкой, заказы не создаются. Так же он выключится сам, если
прайс-лист не пройдёт проверку при запуске — в логах будет строка с причиной.

Что нужно, чтобы включить:

1. Заполнить в `.env` переменные `TBANK_*` (см. `.env.example`).
2. Поправить прайс-лист в блоке `tbank.packs` файла `app/src/main/resources/application.conf`.
   Цены там в **копейках** — ровно в таком виде их ждёт банк.
3. Поднять HTTPS перед приложением и указать вебхук
   `https://<домен>/pay/tbank/notify` в `TBANK_NOTIFICATION_URL`. Сейчас
   приложение слушает голый HTTP, так что нужен обратный прокси с сертификатом.
4. В настройках терминала (или теми же переменными) задать `SuccessURL` и
   `FailURL` — они ведут прямо в диалог с сообществом.

### Сертификат кассы

`securepay.tinkoff.ru` отдаёт сертификат, подписанный корневым CA Минцифры
(«Russian Trusted Root CA»), которого нет ни в стандартных хранилищах ОС, ни в
`cacerts` JVM. Без него первый же запрос к кассе падает на PKIX
(«unable to find valid certification path»), то есть донат не работает вовсе.

Корневой сертификат лежит в `docker/russian_trusted_root_ca.cer` и добавляется
в хранилище JVM при сборке образа, с проверкой отпечатка. Если запускаешь бота
не через Docker — добавь его в `cacerts` сам:

```bash
keytool -importcert -trustcacerts -cacerts -storepass changeit \
  -alias russian-trusted-root-ca -file docker/russian_trusted_root_ca.cer
```

### Как идёт выдача

Выдача дублонов идёт **только** по нотификации банка со статусом `CONFIRMED`, и
ровно один раз: `CONFIRMED` приходит минимум дважды, а кнопка «Проверить оплату»
и фоновая сверка тянут за ту же ручку. Страницы `SuccessURL`/`FailURL` ничего не
начисляют — игрок может открыть их руками.

## Логи по HTTP

Приложение отдаёт свой лог-файл наружу: `GET /logs?tail=500` целиком и
`GET /logs/stream` живым потоком (SSE), оба с необязательным `&grep=...`.

Доступ закрыт паролем `ADMIN_PASSWORD` — тем же, что у админ-панели. Пароль
передаётся заголовком или query-параметром:

```bash
curl -H "Authorization: Bearer $ADMIN_PASSWORD" https://<домен>/logs?tail=500
curl "https://<домен>/logs?tail=500&key=$ADMIN_PASSWORD"
```

Без пароля (и если `ADMIN_PASSWORD` не задан) оба роута отвечают 404. В логах
лежат peer_id игроков, тексты их сообщений и номера заказов доната со суммами —
поэтому открытыми их держать нельзя.
