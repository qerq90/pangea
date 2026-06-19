-- Replace single trauma_name with trauma_names (comma-separated list)
ALTER TABLE heroes ADD COLUMN trauma_names TEXT NOT NULL DEFAULT '';
UPDATE heroes SET trauma_names = COALESCE(trauma_name, '');
ALTER TABLE heroes DROP COLUMN trauma_name;
