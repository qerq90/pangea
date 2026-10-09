-- +goose Up
-- +goose StatementBegin
-- Донат: заказы на покупку дублонов за рубли через кассу Т-Банка.
--
-- Висит на `user_id`, а не на `hero_id`: рестарт персонажа историю платежей не
-- трогает. Цена, количество дублонов и адрес для чека копируются в строку
-- заказа — прайс-лист и почта могут измениться, а выдать нужно ровно то, что
-- игроку обещали в момент оплаты.
CREATE TABLE payments(
  id             BIGSERIAL PRIMARY KEY,
  order_id       TEXT    NOT NULL UNIQUE,
  user_id        BIGINT  NOT NULL,
  sku            TEXT    NOT NULL,
  amount_kopecks BIGINT  NOT NULL,
  doubloons      BIGINT  NOT NULL,
  receipt_email  TEXT    NOT NULL,
  payment_id     TEXT,
  payment_url    TEXT,
  status         TEXT    NOT NULL,
  bank_status    TEXT,
  granted        BOOLEAN NOT NULL DEFAULT FALSE,
  created_at     BIGINT  NOT NULL,
  updated_at     BIGINT  NOT NULL
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_payments_user ON payments(user_id, created_at DESC);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_payments_payment_id ON payments(payment_id);
-- +goose StatementEnd
-- +goose StatementBegin
-- Очередь сверки: незакрытые заказы и оплаченные, но ещё не выданные.
CREATE INDEX idx_payments_unsettled ON payments(updated_at) WHERE NOT granted;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE payments;
-- +goose StatementEnd
