-- +goose Up
-- +goose StatementBegin
-- Почта для кассового чека: игрок присылает её сообщением на экране покупки
-- дублонов (см. ReceiptEmail). Нужна только для чека и больше ни для чего.
ALTER TABLE heroes ADD COLUMN receipt_email TEXT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN receipt_email;
-- +goose StatementEnd
