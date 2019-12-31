import React from "react";
import "./App.css";

const App: React.FC = () => {
  return (
    <div className="board">
      <div className="stack">
        <h4>Title</h4>
        <Card
          imageUrl="https://www.fujifilm.eu/fileadmin/product_migration/dc/samples/ff_x20_008_12.JPG"
          title="My Card"
        />
        <Card
          imageUrl="https://www.fujifilm.eu/fileadmin/product_migration/dc/samples/ff_x20_008_12.JPG"
          title="My Card"
        />
      </div>

      <div className="stack">
        <h4>Title</h4>
        <Card
          imageUrl="https://www.fujifilm.eu/fileadmin/product_migration/dc/samples/ff_x20_008_12.JPG"
          title="My Card"
        />
        <Card
          imageUrl="https://www.fujifilm.eu/fileadmin/product_migration/dc/samples/ff_x20_008_12.JPG"
          title="My Card"
        />
      </div>
    </div>
  );
};

const Card = ({ title, imageUrl }: { title: string; imageUrl: string }) => (
  <div className="card">
    <img className="card-image" width={200} src={imageUrl} alt="some text" />
    <span className="card-title">{title}</span>
  </div>
);

export default App;
