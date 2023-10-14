import {Tag, Descriptions, Space} from 'antd';
import {ConfigProvider} from 'antd';
import enUSIntl from 'antd/lib/locale/en_US';
import {ProDescriptions} from '@ant-design/pro-components';
import {React, useRef} from 'react';


const paper = {
    name: 'Software transactional memory',
    author: 'N Shavit, D Touitou',
    uuid: '123456',
    cite: 'Shavit, Nir, and Dan Touitou. "Software transactional memory." Proceedings of the fourteenth annual ACM symposium on Principles of distributed computing. 1995.',
    url: 'https://dl.acm.org/doi/pdf/10.1145/1400214.1400228',
    lastAccessed: '20131012',
    tags: ['abc', 'def'],
    notes: "#Hello"
}

const columns = [
    {
        title: 'author',
        key: 'author',
        dataIndex: 'author',
        valueType: 'text',
    },
    {
        title: 'uuid',
        key: 'uuid',
        dataIndex: 'uuid',
        valueType: 'text',
        editable: false
    },
    {
        title: 'last accessed',
        key: 'lastAccessed',
        dataIndex: 'lastAccessed',
        valueType: 'date',
    },
    {
        title: 'url',
        key: 'url',
        dataIndex: 'url',
        valueType: 'text',
        render: (dom, entity, index, action) => {
            return (
                <a href={
                    "javascript:window.require('electron').shell.openExternal(" + JSON.stringify(dom) + ")"
                }>{dom}</a>
            );
        },
    },
    {
        title: 'cite',
        key: 'cite',
        dataIndex: 'cite',
        valueType: 'text',
    },
]

const Tags = (paper) => {
    let l = paper.paper.tags.map(
        (x) => {return <Tag style={{fontSize: '120%', padding: '0.3em 0.5em'}}>{'#' + x}</Tag>}
    )
    return <Space size={'middle'} style={{marginBottom: '1em'}}>{l}</Space>
}

const Desc = (paper) => {
    const actionRef = useRef();
    return (
        <ProDescriptions
            actionRef={actionRef}
            bordered
            formProps={{
                onValuesChange: (e, f) => console.log(f),
            }}
            title=""
            request={async () => {return Promise.resolve({success: true, data: paper.paper})}}
            editable={{
            }}
            column={1}
            columns={columns}
        />
    )
}

const Paper = () => {
    return (
        <ConfigProvider locale={enUSIntl}>
            <div style={{fontSize: '120%', padding: '0em 1em'}}>
                <h1>{paper.name}</h1>
                <Tags paper={paper} />
                <Desc paper={paper} />
            </div>
        </ConfigProvider>
    )
}

export default Paper;
