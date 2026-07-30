-- +goose Up
-- NPE в генерации предмета (PassiveKind.poolFor) обрывал FoundItemState.enter уже
-- ПОСЛЕ смены состояния: герой оставался в FoundItem с чужим scene_data, и «Забрать»
-- падало на `DecodingFailure at .item`. Возвращаем таких героев в лабиринт.
UPDATE heroes
   SET state = 'Dungeon'
 WHERE state = 'FoundItem'
   AND (scene_data IS NULL OR jsonb_typeof(scene_data) <> 'object' OR NOT scene_data ? 'item');

-- +goose Down
SELECT 1;
