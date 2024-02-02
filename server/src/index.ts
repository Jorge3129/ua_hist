import Fastify from "fastify";
import { z } from "zod";
import fastifyCors from "@fastify/cors";
import { prisma } from "./prisma";
import { Prisma } from "@prisma/client";

const main = async () => {
  const f = Fastify({
    logger: true,
  });

  f.setErrorHandler(async (err, _req, rep) => {
    if (err instanceof z.ZodError) {
      return rep.status(400).send({
        message: "Invalid request",
        issues: err.issues,
      });
    }
    if (err instanceof Prisma.PrismaClientKnownRequestError) {
      const code = err.code;

      // not found
      if (code === "P2018" || code === "P2025") {
        return rep.status(404).send({
          message: "Not found",
        });
      }
    }

    return rep.status(500).send(err.message);
  });

  f.register(fastifyCors, {
    origin: "*",
  });

  f.post("/", async (req, res) => {
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

    res.status(201).send(savedEvents);
  });

  f.get("/", async () => {
    return await prisma.historyEvent.findMany({
      orderBy: {
        eventIndex: "asc",
      },
    });
  });

  f.delete("/:id", async (req, res) => {
    const schema = z.object({
      id: z.string().uuid(),
    });

    const { id } = schema.parse(req.params);

    await prisma.historyEvent.delete({
      where: {
        id: id,
      },
    });

    res.status(204).send({
      message: "deleted",
    });
  });

  const appPort = 8000;

  f.listen({
    port: appPort,
  });
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
