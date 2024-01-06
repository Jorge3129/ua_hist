import {
  Column,
  CreateDateColumn,
  Entity,
  PrimaryGeneratedColumn,
} from "typeorm";

@Entity()
export class HistoryEvent {
  @PrimaryGeneratedColumn("uuid")
  id: string;

  @Column()
  time: string;

  @Column()
  events: string;

  @Column()
  eventsMarkup: string;

  @Column()
  eventIndex: number;

  @CreateDateColumn()
  createdAt: Date;
}
