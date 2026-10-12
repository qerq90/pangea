-- +goose Up
-- +goose StatementBegin
-- Донат переехал с Т-Банка на Робокассу. Заказы Т-Банка были только тестовыми,
-- а их `payment_id` и статусы (`FormShowed`, `Refunded`) новой кассе ничего не
-- говорят: сверка начала бы спрашивать Робокассу о чужих номерах. Вычищаем.
--
-- Сама таблица остаётся: в `payment_id` теперь лежит `InvId` Робокассы — номер
-- счёта, равный `payments.id`.
DELETE FROM payments;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 1;
-- +goose StatementEnd
