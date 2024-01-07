import { MigrationInterface, QueryRunner } from "typeorm";

export class AddHistEventIndex1704569495765 implements MigrationInterface {
  name = "AddHistEventIndex1704569495765";

  public async up(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(
      `ALTER TABLE "history_event" ADD "eventIndex" integer NOT NULL`
    );
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(
      `ALTER TABLE "history_event" DROP COLUMN "eventIndex"`
    );
  }
}
