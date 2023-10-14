import {React} from 'react';
import {List, Space, Tag} from 'antd';

const papers = [
    {
        name: 'Software Transactional Memory',
        tags: ['Haskell', 'STM']
    },
    {
        name: 'Ant Design Title 2',
        tags: ['aaa']
    },
    {
        name: 'Ant Design Title 3',
        tags: ['aaa']
    },
    {
        name: 'Ant Design Title 4',
        tags: ['asdfsa']
    },
];

const Title = (name) => {
    return <p
        style={{fontSize: '110%', padding: '0em 0em', margin:'0em'}}>
        {name.name}
    </p>
}

const Tags = (tags) => {
    let tags_ = tags.tags.map((x) => {
        return <Tag style={{fontSize: '100%', padding: '0.2em 0.4em'}}>{'#' + x}</Tag>
    });
    return <Space size={'small'}>{tags_}</Space>
}

const PaperList = () => {
    return (
        <List
            itemLayout="horizontal"
            style={{height: '100%', overflow: 'scroll'}}
            dataSource={papers}
            renderItem={(item, index) => (
                <List.Item style={{padding: '1em 1.5em'}}>
                    <List.Item.Meta
                        title=<Title name={item.name} />
                        description=<Tags tags={item.tags} />
                    />
                </List.Item>
            )}
        />
    )
}

export default PaperList;
