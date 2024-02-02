import express, { RequestHandler, ErrorRequestHandler } from "express";
import { z } from "zod";
import cors from "cors";
import { prisma } from "./prisma";

const main = async () => {
  const app = express();

  app.use(express.json());
  app.use(
    cors({
      origin: "*",
    }),
  );

  type ReqHandler = (...p: Parameters<RequestHandler>) => Promise<void>;

  const wrap =
    (fn: ReqHandler): ReqHandler =>
    (...args) =>
      fn(...args).catch((e) => args[2](e));

  app.post(
    "/",
    wrap(async (req, res) => {
      const schema = z.array(
        z.object({
          time: z.string(),
          events: z.string(),
          eventsMarkup: z.string(),
          eventIndex: z.number(),
        }),
      );

      const eventsDto = schema.parse(req.body);

      const savedEvents = await prisma.historyEvent.createMany({
        data: eventsDto,
      });

      res.status(201).json(savedEvents);
    }),
  );

  app.get(
    "/",
    wrap(async (_req, res) => {
      const savedEvents = await prisma.historyEvent.findMany({
        orderBy: {
          eventIndex: "asc",
        },
      });

      res.status(200).json(savedEvents);
    }),
  );

  app.delete(
    "/",
    wrap(async (req, res) => {
      const schema = z.object({
        id: z.string().uuid(),
      });

      const { id } = schema.parse(req.query);

      await prisma.historyEvent.delete({
        where: {
          id: id,
        },
      });

      res.status(204).send();
    }),
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

main()
  .then(async () => {
    await prisma.$disconnect();
  })
  .catch(async (e) => {
    console.error(e);
    await prisma.$disconnect();

    process.exit(1);
  });
