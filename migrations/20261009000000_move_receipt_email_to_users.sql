-- +goose Up
-- +goose StatementBegin
-- Почта для чека принадлежит человеку, а не персонажу: на `heroes` она
-- стиралась вместе с героем при рестарте (deleteHeroCascade), хотя платежи
-- висят на `user_id` и живут дальше. Переносим в `users`.
ALTER TABLE users ADD COLUMN receipt_email TEXT;
-- +goose StatementEnd
-- +goose StatementBegin
UPDATE users u SET receipt_email = h.receipt_email
  FROM heroes h WHERE h.user_id = u.id AND h.receipt_email IS NOT NULL;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN receipt_email;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN receipt_email TEXT;
-- +goose StatementEnd
-- +goose StatementBegin
UPDATE heroes h SET receipt_email = u.receipt_email
  FROM users u WHERE u.id = h.user_id AND u.receipt_email IS NOT NULL;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE users DROP COLUMN receipt_email;
-- +goose StatementEnd
