import { writeFileSync } from "fs";
import dataSource from "../src/data-source";
import { HistoryEvent } from "../src/entity/history-event.entity";
import { join } from "path";

const main = async () => {
  await dataSource.initialize();
  console.log("Data Source initialized");

  await dataSource.runMigrations();
  console.log("DB Schema updated");

  const em = dataSource.manager;

  const records = await em.find(HistoryEvent, { order: { eventIndex: "ASC" } });

  writeFileSync(
    join(__dirname, "./foo.js"),
    `const records = ${JSON.stringify(records.map((r) => r.time))}`
  );

  dataSource.destroy();

  //   process.exit(0);
};

main();
