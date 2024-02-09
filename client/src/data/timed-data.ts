export type TimedData<TData> = [
  data: TData,
  timeInfo: number | [number, number?]
];

export type TimedDataPt<TData> = [
  marker: Omit<TData, "id">,
  timeInfo: number | [number, number?]
];

export const getForEventIndex = <TData>(
  timedData: TimedData<TData>[],
  currentEventIndex: number
): TData[] => {
  return timedData
    .filter(([_, timeInfo]) => {
      if (Array.isArray(timeInfo)) {
        const [start, end] = timeInfo;

        return (
          start <= currentEventIndex &&
          (end === undefined || currentEventIndex < end)
        );
      }

      return currentEventIndex === timeInfo;
    })
    .map(([data]) => data);
};

export function getForEventIndexCurried<TData>(timedData: TimedData<TData>[], currentEventIndex: number): TData[] {
  return timedData
    .filter(([_, timeInfo]) => {
      if (Array.isArray(timeInfo)) {
        const [start, end] = timeInfo;

        return (
          start <= currentEventIndex &&
          (end === undefined || currentEventIndex < end)
        );
      }

      return currentEventIndex === timeInfo;
    })
    .map(([data]) => data);
}
