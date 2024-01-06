import { MigrationInterface, QueryRunner } from "typeorm";

export class AddHistoryEvent1704544764532 implements MigrationInterface {
  name = "AddHistoryEvent1704544764532";

  public async up(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(
      `CREATE TABLE "history_event" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "time" character varying NOT NULL, "events" character varying NOT NULL, "eventsMarkup" character varying NOT NULL, CONSTRAINT "PK_7fbfa45c0a68854e44cba8c6497" PRIMARY KEY ("id"))`
    );
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(`DROP TABLE "history_event"`);
  }
}
