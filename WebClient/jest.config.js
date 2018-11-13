module.exports = {
    transform: {
        '^.+\\.js$': 'babel-jest',
        '^.+\\.html$': 'jest-raw-loader',
        '^.+\\.(html|png|jpg|css)$': '<rootDir>/tests/transformers/fileTransformer.js',
    },
    transformIgnorePatterns: [
        '<rootDir>/node_modules/'
    ],
    setupFiles: [
        '<rootDir>/tests/loadGlobalDependecies.js'
    ],
    modulePaths: [
        '<rootDir>/src/'
    ],
    roots: [
        '<rootDir>/tests/'
    ]
};
