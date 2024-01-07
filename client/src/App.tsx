import { useEffect, useState } from "react";
import "./App.scss";
import { MapSection } from "./components/MapSection";
import _ from "lodash";
import { getMarkers } from "./data/markers";
import { getGeos } from "./data/geojsons";

type HistoryRecord = {
  id: string;
  time: string;
  events: string;
  eventsMarkup: string;
  eventIndex: number;
};

const apiUrl = "http://localhost:8000";

const getAllRecords = async (): Promise<HistoryRecord[]> => {
  return fetch(apiUrl)
    .then((res) => res.json())
    .then((records: HistoryRecord[]) => records.map(replaceLinks));
};

const replaceLinks = (record: HistoryRecord): HistoryRecord => {
  const markup = record.eventsMarkup.replace(
    new RegExp(`href="/wiki`, "g"),
    `href="https://uk.wikipedia.org/wiki`
  );

  return { ...record, eventsMarkup: markup };
};

function App() {
  const [records, setRecords] = useState<HistoryRecord[]>([]);
  const [selectedRecordId, setSelectedRecordId] = useState<string>(
    localStorage.getItem("selectedRecordId") ?? ""
  );

  useEffect(() => {
    getAllRecords().then((resRecords) => {
      setRecords(resRecords);

      if (resRecords.length && !selectedRecordId) {
        setSelectedRecordId(resRecords[0].id);
      }
    });
  }, []);

  useEffect(() => {
    const storedId = localStorage.getItem("selectedRecordId");

    if (selectedRecordId && selectedRecordId !== storedId) {
      localStorage.setItem("selectedRecordId", selectedRecordId);
      return;
    }
  }, [selectedRecordId]);

  const handleRecordClick = (record: HistoryRecord) => {
    setSelectedRecordId(record.id);
    console.log(record.eventIndex);
  };

  const recordsDict = _.keyBy(records, "id");

  const selectedRecord = recordsDict[selectedRecordId];

  const markers = selectedRecord ? getMarkers(selectedRecord.eventIndex) : [];

  const geos = selectedRecord ? getGeos(selectedRecord.eventIndex) : [];

  return (
    <div className="main">
      <div className="map">
        <MapSection markers={markers} geos={geos} />
      </div>
      <div className="records_list">
        {records.map((record) => (
          <div
            className="record"
            key={record.id}
            onClick={() => handleRecordClick(record)}
          >
            <div className="record_time">
              <div>{record.time}</div>
            </div>
            <div
              className="record_desc"
              dangerouslySetInnerHTML={{
                __html: record.eventsMarkup,
              }}
            ></div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default App;
