-- +goose Up
-- +goose StatementBegin
-- Опыт остановлен зельем Фета: пока стоит, опыт не идёт ни из боёв, ни из
-- заданий (см. Hero.expFrozen). Второе зелье его снова запускает.
ALTER TABLE heroes ADD COLUMN exp_frozen BOOLEAN NOT NULL DEFAULT FALSE;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN exp_frozen;
-- +goose StatementEnd
