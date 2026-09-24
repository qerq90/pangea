-- +goose Up
-- +goose StatementBegin
-- Проданные и снятые лоты больше не храним — строка уходит вместе с вещью,
-- поэтому статус и покупатель в таблице лишние: всё, что в ней лежит, либо
-- в продаже, либо ждёт хозяина по истечении срока.
DROP INDEX IF EXISTS auction_lots_status_expires_idx;
ALTER TABLE auction_lots DROP COLUMN IF EXISTS status;
ALTER TABLE auction_lots DROP COLUMN IF EXISTS buyer_id;
CREATE INDEX auction_lots_expires_idx ON auction_lots (expires_at);

-- Выручка, которую не приняла банковская ячейка: ждёт героя в городе, чтобы
-- он не потерял половину, умерев с ней в лабиринте.
CREATE TABLE pending_payouts (
    hero_id   BIGINT PRIMARY KEY REFERENCES heroes (id) ON DELETE CASCADE,
    silver    BIGINT NOT NULL DEFAULT 0,
    doubloons BIGINT NOT NULL DEFAULT 0
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE pending_payouts;
DROP INDEX IF EXISTS auction_lots_expires_idx;
ALTER TABLE auction_lots ADD COLUMN status VARCHAR(16) NOT NULL DEFAULT 'Active';
ALTER TABLE auction_lots ADD COLUMN buyer_id BIGINT;
CREATE INDEX auction_lots_status_expires_idx ON auction_lots (status, expires_at);
-- +goose StatementEnd
