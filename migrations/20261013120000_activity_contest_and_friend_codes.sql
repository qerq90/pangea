-- +goose Up
-- +goose StatementBegin
-- Конкурс активности в группе ВК. Посты группы запоминаем, когда они выходят
-- (событие wall_post_new): в событиях лайков и комментариев даты поста нет, а
-- баллы идут только за свежие посты.
CREATE TABLE group_posts (
    post_id   BIGINT PRIMARY KEY,
    posted_at BIGINT NOT NULL
);

-- Засчитанные действия: лайк и комментарий — по одному на пост от игрока, сколько
-- бы раз он ни ставил и ни снимал отметку. `points` — сколько действие стоило в
-- момент засчёта, `week` — начало недели (мс), в которую оно засчитано: по ним и
-- считаются баллы недели.
CREATE TABLE activity_actions (
    user_id BIGINT      NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    post_id BIGINT      NOT NULL,
    kind    VARCHAR(16) NOT NULL,
    points  BIGINT      NOT NULL,
    week    BIGINT      NOT NULL,
    at      BIGINT      NOT NULL,
    PRIMARY KEY (user_id, post_id, kind)
);

CREATE INDEX activity_actions_week_idx ON activity_actions (week);

-- Подведённые недели: строка появляется в той же транзакции, что и выплаты
-- победителям, — неделю не наградят дважды и не забудут наградить.
CREATE TABLE activity_prizes (
    week    BIGINT PRIMARY KEY,
    paid_at BIGINT NOT NULL
);

-- Код друга: кто чей код ввёл. Ввести код можно один раз — отсюда ключ по
-- `user_id`. `bonus_paid` — заплачено ли владельцу кода за 10 уровень друга.
CREATE TABLE referrals (
    user_id     BIGINT  PRIMARY KEY REFERENCES users (id) ON DELETE CASCADE,
    referrer_id BIGINT  NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    created_at  BIGINT  NOT NULL,
    bonus_paid  BOOLEAN NOT NULL DEFAULT FALSE
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE referrals;
DROP TABLE activity_prizes;
DROP TABLE activity_actions;
DROP TABLE group_posts;
-- +goose StatementEnd
