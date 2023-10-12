import {React} from 'react';

const Paper = (paper) => {
    paper = paper.paper
    return (
        <div>
            <h1>{paper.title}</h1>
        </div>
    )
}

export default Paper;
