-- +goose Up
ALTER TABLE heroes RENAME COLUMN gold TO silver;
ALTER TABLE barrels RENAME COLUMN gold TO silver;
UPDATE heroes SET state = 'SilverVein' WHERE state = 'GoldVein';
UPDATE scheduled_tasks SET expected_state = 'SilverVein' WHERE expected_state = 'GoldVein';

-- +goose Down
ALTER TABLE heroes RENAME COLUMN silver TO gold;
ALTER TABLE barrels RENAME COLUMN silver TO gold;
UPDATE heroes SET state = 'GoldVein' WHERE state = 'SilverVein';
UPDATE scheduled_tasks SET expected_state = 'GoldVein' WHERE expected_state = 'SilverVein';
