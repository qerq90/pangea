-- +goose Up
-- +goose StatementBegin
-- Бой на арене: строка заводится, когда герой записался, и уходит вместе с
-- концом боя. Пока соперника нет, b_user_id пуст, а код ждёт того, кому его
-- передали. Всё изменчивое (эффекты, кулдауны, чей ход) лежит в data.
CREATE TABLE arena_fights (
    id         BIGSERIAL PRIMARY KEY,
    code       TEXT   NOT NULL UNIQUE,
    status     TEXT   NOT NULL,
    a_user_id  BIGINT NOT NULL,
    b_user_id  BIGINT,
    data       JSONB  NOT NULL,
    updated_at BIGINT NOT NULL
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX arena_fights_status_idx ON arena_fights (status, updated_at);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX arena_fights_users_idx ON arena_fights (a_user_id, b_user_id);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE arena_fights;
-- +goose StatementEnd
