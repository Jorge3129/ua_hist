import { MigrationInterface, QueryRunner } from "typeorm";

export class AddCreatedAtHistEvent1704569267399 implements MigrationInterface {
  name = "AddCreatedAtHistEvent1704569267399";

  public async up(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(
      `ALTER TABLE "history_event" ADD "createdAt" TIMESTAMP NOT NULL DEFAULT now()`
    );
  }

  public async down(queryRunner: QueryRunner): Promise<void> {
    await queryRunner.query(
      `ALTER TABLE "history_event" DROP COLUMN "createdAt"`
    );
  }
}
