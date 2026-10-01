-- +goose Up
-- +goose StatementBegin
-- Номер лота больше не берётся из последовательности: проданный или снятый лот
-- освобождает свой, и следующий продавец занимает наименьший свободный
-- (см. AuctionDaoLive.insert). Умолчание снимаем, чтобы вставка без явного id
-- падала сразу, а не выдавала номер, который уже кем-то занят.
ALTER TABLE auction_lots ALTER COLUMN id DROP DEFAULT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
-- Возвращаем последовательность и подводим её за самый большой занятый номер.
ALTER TABLE auction_lots ALTER COLUMN id SET DEFAULT nextval('auction_lots_id_seq');
SELECT setval('auction_lots_id_seq', COALESCE((SELECT max(id) FROM auction_lots), 0) + 1, false);
-- +goose StatementEnd
