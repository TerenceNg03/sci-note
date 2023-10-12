import {React} from 'react';
import {List} from 'antd';

const papers = [
    {
        title: 'Ant Design Title 1',
    },
    {
        title: 'Ant Design Title 2',
    },
    {
        title: 'Ant Design Title 3',
    },
    {
        title: 'Ant Design Title 4',
    },
];


const PaperList = () => {
    return (
        <List
            itemLayout="horizontal"
            style={{height: '100%', overflow: 'scroll'}}
            dataSource={papers}
            renderItem={(item, index) => (
                <List.Item style={{padding: '1em 1.5em'}}>
                    <List.Item.Meta
                        title={item.title}
                        description="Description here"
                    />
                </List.Item>
            )}
        />
    )
}

export default PaperList;
