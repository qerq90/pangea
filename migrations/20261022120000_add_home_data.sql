-- +goose Up
-- +goose StatementBegin
-- Дом героя: какой куплен у Юноны Кинтслев, была ли первая встреча, а дальше —
-- хозяйство дома (см. pangea.model.home.HomeData). Рестарт стирает его вместе
-- с героем.
ALTER TABLE heroes ADD COLUMN home_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN home_data;
-- +goose StatementEnd
