-- +goose Up
-- Пауза «осмотра уровня» убрана: событие лабиринта разыгрывается сразу, без
-- отложенной задачи. Старые Pending-записи с kind='LevelSearch' больше не
-- декодируются в TaskKind и уронили бы поллер — снимаем их.
DELETE FROM scheduled_tasks WHERE kind = 'LevelSearch';

-- +goose Down
SELECT 1;
