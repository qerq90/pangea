-- +goose Up
-- +goose StatementBegin
CREATE TABLE auction_lots (
    id         BIGSERIAL   PRIMARY KEY,
    seller_id  BIGINT      NOT NULL REFERENCES heroes (id) ON DELETE CASCADE,
    item       JSONB       NOT NULL,
    price      BIGINT      NOT NULL,
    currency   VARCHAR(16) NOT NULL,
    status     VARCHAR(16) NOT NULL,
    listed_at  BIGINT      NOT NULL,
    expires_at BIGINT      NOT NULL,
    buyer_id   BIGINT
);

-- Витрина аукциона: активные лоты по сроку годности.
CREATE INDEX auction_lots_status_expires_idx ON auction_lots (status, expires_at);
-- «Мои лоты».
CREATE INDEX auction_lots_seller_idx ON auction_lots (seller_id);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE auction_lots;
-- +goose StatementEnd
