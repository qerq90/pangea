-- +goose Up
-- +goose StatementBegin
-- Переключатели боя, которые герой ставит раз и надолго: сейчас это миазмы
-- тьмы «Некроманта» (порог 10), дальше лягут и другие. Пустой объект читается
-- как «всё по умолчанию» (см. BattlePrefs.default).
ALTER TABLE heroes ADD COLUMN battle_prefs JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN battle_prefs;
-- +goose StatementEnd
