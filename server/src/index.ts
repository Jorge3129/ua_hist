import express, { RequestHandler, ErrorRequestHandler } from "express";
import dataSource from "./data-source";
import zod from "zod";
import { HistoryEvent } from "./entity/history-event.entity";
import cors from "cors";

const main = async () => {
  await dataSource.initialize();
  console.log("Data Source initialized");

  await dataSource.runMigrations();
  console.log("DB Schema updated");

  const em = dataSource.manager;

  const app = express();

  app.use(express.json());
  app.use(
    cors({
      origin: "*",
    })
  );

  type ReqHandler = (...p: Parameters<RequestHandler>) => Promise<void>;

  const wrap =
    (fn: ReqHandler): ReqHandler =>
    (...args) =>
      fn(...args).catch((e) => args[2](e));

  app.post(
    "/",
    wrap(async (req, res) => {
      const schema = zod.array(
        zod.object({
          time: zod.string(),
          events: zod.string(),
          eventsMarkup: zod.string(),
          eventIndex: zod.number(),
        })
      );

      const eventsDto = schema.parse(req.body);

      const savedEvents = await em.save(HistoryEvent, eventsDto);

      res.status(201).json(savedEvents);
    })
  );

  app.get(
    "/",
    wrap(async (_req, res) => {
      const savedEvents = await em.find(HistoryEvent, {
        order: {
          eventIndex: "ASC",
        },
      });

      res.status(200).json(savedEvents);
    })
  );

  app.delete(
    "/",
    wrap(async (_req, res) => {
      await em.delete(HistoryEvent, {});

      res.status(204).send();
    })
  );

  app.use((...args: Parameters<ErrorRequestHandler>) => {
    const [err, _req, res] = args;

    console.log({ err });

    res.status(500).json({
      error: err.message,
    });
  });

  const appPort = 8000;

  app.listen(appPort, () => console.log(`Server running on port ${appPort}`));
};

main();
